{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getZonedTime)
import Options.Applicative hiding ((<>), switch)
import qualified Options.Applicative as Opt
import Text.Read (readMaybe)

import Web.Telegram.Bot.DSL
import Web.Telegram.Bot.Server

helpMessage :: Text
helpMessage = (
       "This is the example bot. Try using /echo <msg> and /reverse <msg>. \n"
    <> "You could also try the calculator, /add.\n"
    <> "For a full list of commands, use /help."
  )

bot :: MonadIO io => Bot io ()
bot = choice $ cmd "start"   (const (send helpMessage))
            <> cmd "echo"    (send)
            <> cmd "reverse" (send . T.reverse)
            <> cmd "add" (\_ -> do
                 send $  "Entering the adder! \n"
                      <> "Enter a number!"
                 xi <- input' Nothing 5
                 send $  "Received " <> tshow xi <> "\n"
                      <> "Enter another!"
                 yi <- input' Nothing 5
                 let sum = (+) <$> xi <*> yi :: Maybe Integer
                 send $ "Received " <> tshow yi <> "\n"
                     <> "Sum is " <> tshow sum)
            <> cmd "date" (\_ -> do
                 t <- liftIO $ getZonedTime
                 send $ "Current time: " <> (T.pack . show $ t))

-- Helpers

input' :: MonadIO io => Read a => Maybe Text -> Int -> Bot io (Maybe a)
input' m i = loop i
  where loop i'
         | i' <= 0   = pure Nothing
         | otherwise = do
            maybe (pure ()) send m
            x <- readMaybe . T.unpack <$> input
            case x of
              Just v  -> pure (Just v)
              Nothing -> loop (i' - 1)

tshow :: Show a => a -> Text
tshow = T.pack . show

-- Application entry point
main :: IO ()
main = do
  args <- execParser opts
  let runner = case args of
                  Polling token botEnv -> runBotWithPolling token botEnv
                  Webhook botSettings  -> runBotWithWebhooks botSettings
  runner bot


data ProgramCommand = Polling Text BotEnv
                    | Webhook BotSettings

opts = info (helper <*> commandParser) fullDesc

(<+>) :: Monoid a => a -> a -> a
x <+> y = x `mappend` y

commandParser :: Parser ProgramCommand
commandParser =
  subparser $ command "polling"
                      (info pollingOptions
                            (progDesc "Run the bot using the polling method."))
          <+> command "webhooks"
                      (info webhookOptions
                            (progDesc "Run the bot using webhooks."))

pollingOptions :: Parser ProgramCommand
pollingOptions = Polling <$> argument text (metavar "BOT_TOKEN")
                         <*> botEnv

sslOptions :: Parser BotCertSettings
sslOptions =
  BotCertSettings <$> strOption (long "cert"
                                 Opt.<> metavar "SSL_CERT_FILE")
                  <*> (Just <$> strOption (long "key"
                                           Opt.<> metavar "SSL_KEY_FILE"))

webhookOptions :: Parser ProgramCommand
webhookOptions =
  Webhook <$> (BotSettings <$> optional sslOptions
                           <*> argument text        (metavar "REMOTE_URL_BASE")
                           <*> argument auto        (metavar "PORT")
                           <*> argument text        (metavar "BOT_TOKEN")
                           <*> pure Nothing
                           <*> botEnv)

botEnv = BotEnv <$> argument text (metavar "BOT_USER_NAME")

text = T.pack <$> str
