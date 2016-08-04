{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram.Bot.Server
  ( BotSettings(..)
  , runBot ) where

import Web.Telegram.Bot.DSL
import Web.Telegram.API.Bot

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client      (Manager, newManager)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.Wai (Application)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp (Settings, setPort, setLogger, defaultSettings)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Servant
import System.Random



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

type URL = Text
type WebhookApi =
  Capture "secret" Text :> ReqBody '[JSON] Update
                        :> Post '[JSON] ()

data BotConf = BotConf
  {
    bcSecret  :: Text,
    bcBot     :: Bot,
    bcToken   :: Token,
    bcManager :: Manager
  }
type BotM = ReaderT BotConf (ExceptT ServantErr IO)

webhookServer :: Text -> Update -> BotM ()
webhookServer secret value = do
  presetSecret <- bcSecret <$> ask
  bot <- bcBot <$> ask
  if presetSecret /= secret
    then (liftIO $ putStrLn "Invalid secret")
    else runBotOnUpdate bot value

webhookApi :: Proxy WebhookApi
webhookApi = Proxy :: Proxy WebhookApi

webhookApp :: BotConf -> Application
webhookApp bc =
  serve webhookApi $ enter (runReaderTNat bc) (webhookServer :: ServerT WebhookApi BotM)


data BotSettings = BotSettings
  { bsSslCert        :: FilePath
  , bsSslKey         :: FilePath
  , bsRemoteUrl      :: Text
  , bsListeningPort  :: Int
  , bsToken          :: Text
  , bsServerSettings :: Maybe Settings }

runBot :: (MkBot bot) => BotSettings -> bot -> IO ()
runBot s b = do
  let tls = tlsSettings (bsSslCert s) (bsSslKey s)
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
        runBot' tls (Just secret) = withStdoutLogger $ \log -> do
          let settings = setPort (bsListeningPort s)
                       . setLogger log
                       . maybe defaultSettings id
                       $ (bsServerSettings s)
          replyManager <- newManager tlsManagerSettings
          let bc = BotConf secret (mkBot b) (Token $ bsToken s) replyManager
          runTLS tls settings (webhookApp bc)

