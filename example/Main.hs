{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Web.Telegram.Bot.DSL
import Web.Telegram.Bot.Server

helpMessage :: Text
helpMessage = "Use \"/echo <msg>\" or \"/reverse <msg>\" to test this bot!"

bot :: Bot
bot =  cmd "/echo"    Just
   <+> cmd "/reverse" (Just . T.reverse)
   <+> cmd "/help"    (const (Just helpMessage))

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
  BotSettings <$> argument str  (metavar "SSL_CERT_FILE")
              <*> argument str  (metavar "SSL_KEY_FILE")
              <*> argument text (metavar "REMOTE_URL_BASE")
              <*> argument auto (metavar "PORT")
              <*> argument text (metavar "BOT_TOKEN")
              <*> pure Nothing
  where text = T.pack <$> str
