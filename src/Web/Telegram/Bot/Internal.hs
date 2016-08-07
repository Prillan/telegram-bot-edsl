{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram.Bot.Internal
 ( Bot
 , BotEnv(..)
 , env
 , BotInput(..)
 , BotOutput(..)
 , send
 , cmd
 , cancel
 , input
 , inputWithReply
 , mkBot
 , BotM
 , BotCommand
 , runBotInTerminal ) where

import Control.Monad (join, foldM, forever)
import Control.Monad.Trans.Free
import Control.Monad.Trans.Reader
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Pipes
import qualified Pipes.Prelude as P

type BotCommand next = [(Text, Text -> next)]

type MessageId = Int
data BotAction next = BASend Text (Maybe MessageId) next
                    | BAInput (BotInput -> next)
                    | BACancel
 deriving Functor

data BotEnv = BotEnv { botName :: Text }
  deriving Show

type BotM m = FreeT BotAction (ReaderT BotEnv m)

data BotInput = BotTextMessage { botTextMessageText   :: Text
                               , botTextMessageId     :: MessageId
                               , botTextMessageUserId :: Int }
  deriving Show
data BotOutput = BotSend Text (Maybe MessageId)
  deriving Show

type Bot m = Proxy () BotInput () BotOutput m

cmd :: Monad m => Text -> (Text -> BotM m a) -> BotCommand (BotM m a)
cmd t c = [(t, c)]

send' :: Monad m => Text -> Maybe MessageId -> BotM m ()
send' t m = liftF (BASend t m ())

send :: Monad m => Text -> BotM m ()
send t = send' t Nothing

reply :: Monad m => Text -> MessageId -> BotM m ()
reply t i = send' t (Just i)

cancel :: Monad m => BotM m ()
cancel  = liftF BACancel

input' :: Monad m => BotM m BotInput
input' = liftF (BAInput id)

input :: Monad m => BotM m Text
input = botTextMessageText <$> input'

inputWithReply :: Monad m => BotM m (Text, (Text -> BotM m ()))
inputWithReply = go <$> input'
  where go (BotTextMessage t mid _) = (t, flip reply mid)

env :: Monad m => BotM m BotEnv
env = lift ask

mkBot :: (MonadIO m) => BotEnv -> BotM m a -> Bot m ()
mkBot e bot = iterTM go (hoistFreeT (flip runReaderT e) bot *> pure ())
  where go BACancel = l "Cancel" *> s "Alright, cancelling." Nothing *> pure ()
        go (BASend t mid n) = s t mid *> n
        go (BAInput n) = await >>= n
        s t mid
          | T.null t  = l "Empty message, skipping."
          | otherwise = yield (BotSend t mid)
        l t = liftIO $ putStrLn $ T.unpack t

fakeIn :: Producer BotInput IO ()
fakeIn = loop 0
  where loop mid = do
          liftIO $ putStr "Write something to the bot: "
          x <- liftIO $ T.pack <$> getLine
          if x == "q"
             then pure ()
             else yield (BotTextMessage x mid 0) *> loop (mid + 1)

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

runBotInTerminal :: BotM IO () -> IO ()
runBotInTerminal bot = do
  putStrLn "Starting bot."
  let env = BotEnv "test_bot"
  putStrLn "With environment: "
  putStrLn $ (" " ++) $ show env
  putStrLn "Press q at any time to quit"
  runEffect $ fakeIn >-> (mkBot env (forever bot)) >-> fakeOut
