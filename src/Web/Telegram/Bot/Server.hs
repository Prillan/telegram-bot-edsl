{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram.Bot.Server
  ( BotSettings(..)
  , runBot ) where

import Web.Telegram.Bot.Internal hiding (send)
import Web.Telegram.API.Bot

import Control.Concurrent.Async (async)
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client      (Manager, newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.Wai (Application)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp (Settings, setPort, setLogger, defaultSettings, runSettings)
import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS, tlsSettings)
import Pipes hiding (Proxy)
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Servant
import System.Random

sendReply :: (Monad m, MonadIO io) => ChatId -> Text -> ReaderT (BotConf m) io ()
sendReply cid t = do
  manager <- bcManager <$> ask
  token   <- bcToken   <$> ask
  let smr = SendMessageRequest
             {
               message_chat_id = T.pack.show $ cid
             , message_text    = t
             , message_parse_mode = Nothing
             , message_disable_web_page_preview = Nothing
             , message_disable_notification = Nothing
             , message_reply_to_message_id = Nothing
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

data BotConf m = BotConf
  {
    bcSecret  :: Text,
    bcBot     :: Bot m (),
    bcToken   :: Token,
    bcManager :: Manager
  }

type ChatId = Int
type Callback = ServerOutput -> IO ()
data ServerOutput = ServerOutput ChatId Text

webhookServer :: MonadIO io
              => Callback
              -> Text
              -> Update
              -> ReaderT (BotConf io) Handler ()
webhookServer callback secret value = do
  x <- bcSecret <$> ask
  when (x == secret) $ do
    let m = message value
    case (chat_id.chat <$> m, m >>= text) of
      (Just cid, Just t)  -> liftIO $ callback $ ServerOutput cid t
      _ -> pure ()

webhookApi :: Proxy WebhookApi
webhookApi = Proxy :: Proxy WebhookApi

webhookApp :: MonadIO m => BotConf m -> Callback -> Application
webhookApp bc callback = do
   let webhookServer' = webhookServer callback
   serve webhookApi $ enter (runReaderTNat bc) webhookServer'

botWorker :: MonadIO io => Bot IO ()
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

handleResponses :: ReaderT (BotConf IO) (Consumer (ChatId, BotOutput) IO) ()
handleResponses = forever $ do
  (cid, a) <- lift $ await
  case a of
    BotSend t -> sendReply cid t
    _ -> pure ()
  liftIO $ putStrLn $ "RESPONSE: " <> show a

runServer :: Maybe TLSSettings -> Settings -> BotConf IO -> IO ()
runServer tls settings bc = do
  (serverOutput, serverInput) <- liftIO $ spawn (bounded 1)
  let cb d = atomically $ send serverOutput d *> pure ()
  (botOutput, botInput) <- liftIO $ spawn unbounded
  async $ runEffect $ fromInput serverInput >-> botWorker (bcBot bc) botOutput
  async $ runEffect $ fromInput botInput    >-> (runReaderT handleResponses bc)
  case tls of
    Just tls' -> runTLS tls' settings (webhookApp bc cb)
    Nothing   -> runSettings settings (webhookApp bc cb)

data BotSettings = BotSettings
  { bsSslCert        :: FilePath
  , bsSslKey         :: Maybe FilePath
  , bsRemoteUrl      :: Text
  , bsListeningPort  :: Int
  , bsToken          :: Text
  , bsServerSettings :: Maybe Settings
  , bsBotEnv         :: BotEnv }

runBot :: BotSettings -> BotM IO () -> IO ()
runBot s b = do
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
          replyManager <- newManager tlsManagerSettings
          let bc = BotConf secret (mkBot (bsBotEnv s) b) (Token $ bsToken s) replyManager
          runServer tls settings bc

