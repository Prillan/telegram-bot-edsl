{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram.Bot.DSL
  ( send
  , cmd
  , choice
  , choiceNoHelp
  , choiceCustomHelp
  , input
  , mkBot
  , Bot
  , BotM
  , BotCommand
  , runBotInTerminal
  , (<>)
  , MonadIO
  , liftIO ) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Web.Telegram.Bot.Internal

choice :: Monad m => BotCommand (BotM m a) -> BotM m a
choice = choice' (Just defaultHelpMessage)

choiceNoHelp :: Monad m => BotCommand (BotM m a) -> BotM m a
choiceNoHelp = choice' Nothing

choiceCustomHelp :: Monad m => ([Text] -> Text) -> BotCommand (BotM m a) -> BotM m a
choiceCustomHelp help = choice' (Just help)

defaultHelpMessage :: [Text] -> Text
defaultHelpMessage cmds =
  T.intercalate "\n" $ (["Valid commands: "] ++ cmds ++ ["/help"])

choice' :: Monad m => (Maybe ([Text] -> Text)) -> BotCommand (BotM m a) -> BotM m a
choice' helpMessage cmds = do
  x <- input
  case (helpMessage, x == "/help") of
    (Just help, True) -> send (help $ map fst cmds) *> choice cmds
    _                 -> foldM (go x) Nothing cmds >>= maybe (choice cmds) pure
  where go t Nothing (cp, c)
          | cp `T.isPrefixOf` t = Just <$> c (parseInput cp t)
          | otherwise           = pure Nothing
        go _ v _ = pure v
        parseInput prefix message = T.drop (T.length prefix + 1) message
