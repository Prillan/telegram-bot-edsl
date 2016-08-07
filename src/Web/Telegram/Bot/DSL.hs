{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram.Bot.DSL
  ( send
  , cmd
  , choice
  , choiceNoHelp
  , choiceCustomHelp
  , input
  , inputWithReply
  , mkBot
  , Bot
  , BotEnv(..)
  , env
  , BotM
  , BotCommand
  , runBotInTerminal
  , (<>)
  , MonadIO
  , liftIO ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor
import Data.Char
import Data.Maybe
import Data.Semigroup ((<>))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as MP
import Web.Telegram.Bot.Internal

choice :: Monad m => BotCommand (BotM m a) -> BotM m a
choice = choice' (Just defaultHelpMessage)

choiceNoHelp :: Monad m => BotCommand (BotM m a) -> BotM m a
choiceNoHelp = choice' Nothing

choiceCustomHelp :: Monad m => ([Text] -> Text) -> BotCommand (BotM m a) -> BotM m a
choiceCustomHelp help = choice' (Just help)

defaultHelpMessage :: [Text] -> Text
defaultHelpMessage cmds =
  T.intercalate "\n" $ (["Valid commands: "] ++ map ("/" <>) cmds ++ ["/help"])

data Command = Command { command     :: Text
                       , botUserName :: Maybe Text
                       , arguments   :: Maybe Text }
  deriving Show
commandParser :: Parsec Text Command
commandParser = do
  _ <- MP.char '/'
  cmd <- some (MP.satisfy ((&&) <$> not.isSeparator <*> not.('@'==)))
  name <- optional (MP.char '@'
                 *> some (MP.satisfy (not.isSeparator)))
  args <- optional (MP.spaceChar *> some MP.anyChar)
  MP.eof
  pure (Command (T.pack cmd) (T.pack <$> name) (T.pack <$> args))

parseCommand :: Text -> Either Text Command
parseCommand = first (T.pack.show) . MP.runParser commandParser ""

choice' :: Monad m => (Maybe ([Text] -> Text)) -> BotCommand (BotM m a) -> BotM m a
choice' helpMessage cmds = loop
  where loop = do
          x <- input
          e <- env
          maybe loop id $ do
            c <- either (const Nothing) Just $ parseCommand x
            let un = botUserName c
            guard (isNothing un || un == Just (botName e))
            pure $ case (helpMessage, command c) of
                      (Just help, "help") -> send (help $ map fst cmds) *> loop
                      _                   -> foldM (go c) Nothing cmds
                                               >>= maybe (choice cmds) pure
        go ic Nothing (cp, cmd)
          | command ic == cp = Just <$> cmd (maybe "" id (arguments ic))
          | otherwise        = pure Nothing
        go _ v _ = pure v
