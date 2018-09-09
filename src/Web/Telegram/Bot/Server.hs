{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram.Bot.Server
  ( BotSettings(..)
  , runBotWithWebhooks
  , runBotWithPolling ) where

import Web.Telegram.Bot.Internal hiding (send)
import Web.Telegram.API.Bot

import Control.Concurrent.Async (async)
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client      (Manager, newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.Wai (Application)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp ( Settings
                                , setPort
                                , setLogger
                                , defaultSettings
                                , runSettings )
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS, tlsSettings)
import Pipes hiding (Proxy)
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Servant
import System.Random

sendReply :: MonadIO io
          => ChatId
          -> Text
          -> Maybe Int
          -> ReaderT (Manager, Token) io ()
sendReply cid t mid = do
  (manager, token) <- ask
  let smr = SendMessageRequest
             {
               message_chat_id = T.pack.show $ cid
             , message_text    = t
             , message_parse_mode = Nothing
             , message_disable_web_page_preview = Nothing
             , message_disable_notification = Nothing
             , message_reply_to_message_id = mid
             , message_reply_markup = Nothing
             }
  response <- liftIO $ sendMessage token smr manager
  case response of
    Left e -> liftIO $ putStrLn $ take 1000 $ show e
    Right _ -> liftIO $ putStrLn $ "Message sent successfully"

generateSecretToken :: Int -> IO Text
generateSecretToken n = T.map removeBad
                      . decodeUtf8
                      . B64.encode
                      . B.pack
                      . take n
                      . randoms <$> getStdGen
  where removeBad '/' = '_'
        removeBad '+' = '_'
        removeBad '=' = '_'
        removeBad x   = x

type WebhookApi =
  Capture "secret" Text :> ReqBody '[JSON] Update
                        :> Post '[JSON] ()

type ChatId = Int
type Callback = ServerOutput -> IO ()
data ServerOutput = ServerOutput ChatId BotInput

parseUpdate :: Update -> Maybe ServerOutput
parseUpdate value = do
  m <- message value
  t <- text m
  let mid = message_id m
      cid = chat_id (chat m)
  uid <- user_id <$> from m
  pure $ ServerOutput cid (BotTextMessage t mid uid)

highestUpdateId :: Either a UpdatesResponse -> Maybe Int
highestUpdateId response =
  case update_result <$> response of
    Right [] -> Nothing
    Right xs -> Just (maximum (map update_id xs))
    Left _   -> Nothing

webhookServer :: Callback
              -> Text
              -> Text
              -> Update
              -> Handler ()
webhookServer callback presetSecret secret value =
  maybe (pure ()) (liftIO . callback) $ do
    guard (presetSecret == secret)
    parseUpdate value

webhookApi :: Proxy WebhookApi
webhookApi = Proxy :: Proxy WebhookApi

webhookApp :: Text -> Callback -> Application
webhookApp secret callback = do
   serve webhookApi $ webhookServer callback secret

webhookServerProducer :: Maybe TLSSettings
                      -> Settings
                      -> Text
                      -> Output ServerOutput
                      -> IO ()
webhookServerProducer tls settings secret serverOutput =
  case tls of
    Just tls' -> runTLS tls' settings (webhookApp secret cb)
    Nothing   -> runSettings settings (webhookApp secret cb)
  where cb d = atomically $ send serverOutput d *> pure ()

longPollingProducer :: Text
                    -> Output ServerOutput
                    -> IO ()
longPollingProducer token serverOutput = do
      manager <- newManager tlsManagerSettings
      loop manager Nothing
    where send' = liftIO . atomically . send serverOutput
          loop m offset = do
            putStrLn "Starting long poll..."
            response <- getUpdates (Token $ token) offset Nothing (Just 60) m
            putStrLn "Reponse received."
            case response of
              Right (UpdatesResponse updates) ->
                mapM_ send' . mapMaybe parseUpdate $ updates
              Left e ->
                putStrLn $ ("Received invalid updates: \n" ++) . take 100 . show $ e
            let maxOffset = max (highestUpdateId response) offset
                newOffset = (+1) <$> maxOffset
            loop m newOffset

botWorker :: MonadIO io => BotPipe IO ()
                        -> Output (ChatId, BotOutput)
                        -> Consumer ServerOutput io ()
botWorker defaultBot output = loop Map.empty
  where loop m = do
          sm  <- await  -- Get message from the server
          let ServerOutput cid t = sm
          (b, m') <- case Map.lookup cid m of
                       Nothing -> do
                         (bo, bi) <- liftIO $ spawn unbounded
                         liftIO $ async $ runEffect $ fromInput bi
                                                   >-> forever defaultBot
                                                   >-> P.map (\a -> (cid, a))
                                                   >-> toOutput output

                         pure (bo, Map.insert cid bo m)
                       Just mbox -> pure (mbox, m)
          liftIO $ atomically $ send b t -- Send a message to the specific bot
          loop m'

handleResponses :: ReaderT (Manager, Token) (Consumer (ChatId, BotOutput) IO) ()
handleResponses = forever $ do
  (cid, a) <- lift $ await
  case a of
    BotSend t mid -> sendReply cid t mid
  liftIO $ putStrLn $ "RESPONSE: " <> show a

runBot :: BotPipe IO () -> Token -> (Output ServerOutput -> IO ()) -> IO ()
runBot defaultBot token producer = do
  (serverOutput, serverInput) <- spawn (bounded 1)
  (botOutput, botInput)       <- spawn unbounded
  replyManager                <- newManager tlsManagerSettings
  async $ runEffect $ fromInput serverInput
                      >-> botWorker defaultBot botOutput
  async $ runEffect $ fromInput botInput
                      >-> (runReaderT handleResponses (replyManager, token))
  producer serverOutput

data BotSettings = BotSettings
  { bsSslCert        :: FilePath
  , bsSslKey         :: Maybe FilePath
  , bsRemoteUrl      :: Text
  , bsListeningPort  :: Int
  , bsToken          :: Text
  , bsServerSettings :: Maybe Settings
  , bsBotEnv         :: BotEnv }

runBotWithWebhooks :: BotSettings -> Bot IO () -> IO ()
runBotWithWebhooks s b = do
  let tls = tlsSettings <$> (pure $ bsSslCert s) <*> (bsSslKey s)
      req = SetWebhookWithCertRequest
            $ FileUpload Nothing (FileUploadFile (bsSslCert s))
  manager <- newManager tlsManagerSettings
  bracket (setupHook req manager) (destroyHook manager) (runBot' tls)
  where setupHook req manager = do
          secret <- generateSecretToken 32
          let url = bsRemoteUrl s <> "/" <> secret
          res <- setWebhookWithCert (Token $ bsToken s) (Just $ url) req manager
          case res of
            Left e -> do
              putStrLn "Failed to set webhook."
              putStrLn $ take 100 $ show e
              pure $ Nothing
            Right _ -> do
              putStrLn . T.unpack $ "WebHook set at " <> url
              pure $ Just secret
        destroyHook manager _ = do
          res <- setWebhook (Token $ bsToken s) Nothing manager
          case res of
            Left e -> do
              putStrLn "Failed to unset webhook"
              putStrLn $ take 100 $ show e
            Right _ -> do
              putStrLn $ "Successfully removed the webhook"
        runBot' _ Nothing = pure ()
        runBot' tls (Just secret) = withStdoutLogger $ \aplog -> do
          let settings = setPort (bsListeningPort s)
                       . setLogger aplog
                       . maybe defaultSettings id
                       $ (bsServerSettings s)
          runBot (mkBot (bsBotEnv s) b)
                 (Token $ bsToken s)
                 (webhookServerProducer tls settings secret)

runBotWithPolling :: Text -> BotEnv -> Bot IO () -> IO ()
runBotWithPolling token botEnv bot =
  runBot (mkBot botEnv bot)
         (Token token)
         (longPollingProducer token)
