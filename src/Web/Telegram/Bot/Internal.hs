{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Telegram.Bot.Internal
 ( Bot
 , BotOutput(..)
 , send
 , cmd
 , cancel
 , input
 , mkBot
 , BotM
 , BotCommand
 , runBotInTerminal ) where

import Control.Monad (join, foldM, forever)
import Control.Monad.Trans.Free
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Pipes
import qualified Pipes.Prelude as P

type BotCommand next = [(Text, Text -> next)]

data BotAction next = BASend Text next
                    | BAInput (Text -> next)
                    | BACancel
 deriving Functor

type BotM m = FreeT BotAction m

data BotOutput = BotSend Text
  deriving Show

type Bot m = Proxy () Text () BotOutput m

cmd :: Monad m => Text -> (Text -> BotM m a) -> BotCommand (BotM m a)
cmd t c = [(t, c)]

send :: Monad m => Text -> BotM m ()
send t = liftF (BASend t ())

cancel :: Monad m => BotM m ()
cancel  = liftF BACancel

input :: Monad m => BotM m Text
input = liftF (BAInput id)

mkBot :: (MonadIO m) => BotM m a -> Bot m ()
mkBot bot = iterTM go (bot *> pure ())
  where go BACancel = l "Cancel" *> s "Alright, cancelling." *> pure ()
        go (BASend t n) = s t *> n
        go (BAInput n) = await >>= n
        s t
          | T.null t  = l "Empty message, skipping."
          | otherwise = yield (BotSend t)
        l t = liftIO $ putStrLn $ T.unpack t

fakeIn :: Producer Text IO ()
fakeIn = do
  liftIO $ putStr "Write something to the bot: "
  x <- liftIO $ T.pack <$> getLine
  if x == "q"
     then pure ()
     else yield x *> fakeIn

fakeOut = do
  x <- await
  case x of
    BotSend t -> liftIO $ putStrLn . T.unpack $ "** START MESSAGE ** \n"
                                              <> t
                                              <> "\n** END MESSAGE **"
  fakeOut

runBotInTerminal :: BotM IO () -> IO ()
runBotInTerminal bot = do
  putStrLn "Starting bot."
  putStrLn "Press q at any time to quit"
  runEffect $ fakeIn >-> (mkBot (forever bot)) >-> fakeOut
