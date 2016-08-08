{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram.Bot.Internal
 ( Bot
 , BotEnv(..)
 , BotPipe
 , env
 , BotInput(..)
 , BotOutput(..)
 , send
 , cmd
 , input
 , inputWithReply
 , mkBot
 , BotCommand
 , runBotInTerminal ) where

import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Pipes

type BotCommand next = [(Text, Text -> next)]

type MessageId = Int

data BotEnv = BotEnv { botName :: Text }
  deriving Show

data BotInput = BotTextMessage { botTextMessageText   :: Text
                               , botTextMessageId     :: MessageId
                               , botTextMessageUserId :: Int }
  deriving Show
data BotOutput = BotSend Text (Maybe MessageId)
  deriving Show

type Bot m = Proxy () BotInput () BotOutput (ReaderT BotEnv m)
type BotPipe m = Proxy () BotInput () BotOutput m

cmd :: Monad m => Text -> (Text -> Bot m a) -> BotCommand (Bot m a)
cmd t c = [(t, c)]

send' :: Monad m => Text -> Maybe MessageId -> Bot m ()
send' t m = yield (BotSend t m)

send :: Monad m => Text -> Bot m ()
send t = send' t Nothing

reply :: Monad m => Text -> MessageId -> Bot m ()
reply t i = send' t (Just i)

input' :: Monad m => Bot m BotInput
input' = await

input :: Monad m => Bot m Text
input = botTextMessageText <$> input'

inputWithReply :: Monad m => Bot m (Text, (Text -> Bot m ()))
inputWithReply = go <$> input'
  where go (BotTextMessage t mid _) = (t, flip reply mid)

env :: Monad m => Bot m BotEnv
env = lift ask

mkBot :: MonadIO m
      => BotEnv
      -> Bot m a
      -> BotPipe m ()
mkBot e bot = hoist (flip runReaderT e) bot *> pure ()

fakeIn :: Producer BotInput IO ()
fakeIn = loop 0
  where loop mid = do
          liftIO $ putStr "Write something to the bot: "
          x <- liftIO $ T.pack <$> getLine
          if x == "q"
             then pure ()
             else yield (BotTextMessage x mid 0) *> loop (mid + 1)

fakeOut :: Consumer BotOutput IO ()
fakeOut = do
  x <- await
  liftIO $ putStrLn . T.unpack $
    case x of
      BotSend t (Just i) -> "** START MESSAGE, REPLY TO "
                         <> T.pack (show i)
                         <> "** \n"
                         <> t
                         <> "\n** END MESSAGE **"
      BotSend t _        -> "** START MESSAGE **\n"
                         <> t
                         <> "\n** END MESSAGE **"
  fakeOut

runBotInTerminal :: Bot IO () -> IO ()
runBotInTerminal bot = do
  putStrLn "Starting bot."
  let env = BotEnv "test_bot"
  putStrLn "With environment: "
  putStrLn $ (" " ++) $ show env
  putStrLn "Press q at any time to quit"
  runEffect $ fakeIn >-> (mkBot env (forever bot)) >-> fakeOut
