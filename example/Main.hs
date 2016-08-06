{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getZonedTime)
import Options.Applicative hiding ((<>), switch)
import Text.Read (readMaybe)

import Web.Telegram.Bot.DSL
import Web.Telegram.Bot.Server

helpMessage :: Text
helpMessage = (
       "This is the example bot. Try using /echo <msg> and /reverse <msg>. \n"
    <> "You could also try the calculator, /add."
  )

bot :: MonadIO io => BotM io ()
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

input' :: MonadIO io => Read a => Maybe Text -> Int -> BotM io (Maybe a)
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
 runBot args bot

-- Option parsing
opts :: ParserInfo BotSettings
opts = info (helper <*> options) fullDesc

options :: Parser BotSettings
options =
  BotSettings <$> argument str            (metavar "SSL_CERT_FILE")
              <*> argument (Just <$> str) (metavar "SSL_KEY_FILE")
              <*> argument text           (metavar "REMOTE_URL_BASE")
              <*> argument auto           (metavar "PORT")
              <*> argument text           (metavar "BOT_TOKEN")
              <*> pure Nothing
  where text = T.pack <$> str
