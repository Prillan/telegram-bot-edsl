{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Telegram.Bot.DSL where
  -- ( Action(..)
  -- , Bot(..)
  -- , Listener(..)
  -- , MkBot(..)
  -- , (<+>)
  -- , msg
  -- , cmd ) 

import Data.Bifunctor (bimap)
import Control.Monad.Cont
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Free
import Data.Maybe (maybeToList)
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import Data.Time (getZonedTime)
import qualified Data.Text as T
import Web.Telegram.API.Bot ( Update(..)
                            , Message(..) )

import Pipes
import Pipes.Core (request)


type BotCommand next = [(Text, Text -> next)]

data BotAction next = BAWait (BotCommand next)
                    | BASend Text next
                    | BACancel
 deriving Functor

type Bot m = FreeT BotAction m

cmd :: Monad m => Text -> (Text -> Bot m a) -> BotCommand (Bot m a)
cmd t c = [(t, c)]

wait' :: Monad m => BotCommand a -> Bot m a
wait' tc = liftF (BAWait tc)

wait :: Monad m => BotCommand (Bot m a) -> Bot m a
wait = join . wait'

send :: Monad m => Text -> Bot m ()
send t = liftF (BASend t ())

cancel :: Monad m => Bot m ()
cancel  = liftF BACancel

mkPipesBot :: MonadIO m => Bot m a -> Proxy () Text y' y m ()
mkPipesBot bot = iterTM go (bot *> pure ())
  where go BACancel = p "Cancel" *> pure ()
        go (BASend t n) = p ("Sending message: " <> t) *> n
        go (BAWait cmds) = do
          x <- await
          let check True  _ = pure True
              check False (t, c)
                | t `T.isPrefixOf` x = c (parseInput t x) *> pure True
                | otherwise = pure False
          handled <- foldM check False cmds
          if handled
             then pure ()
             else p "Valid commands: "
                  *> mapM_ (p.fst) cmds
                  *> go (BAWait cmds)

        p = liftIO . putStrLn . T.unpack

-- TODO: Make mkPipesBot execute BASend instead of (liftIO . putStrLn . T.unpack)

parseInput :: Text -> Text -> Text
parseInput prefix message = T.drop (T.length prefix + 1) message

testBot :: (MonadIO m, Monad m) => Bot m ()
testBot = do
  wait $ cmd "/echo"    send
      <> cmd "/reverse" (send . T.reverse)
      <> cmd "/nested" (\_ -> do
                     send "Inside a nested expression"
                     send "New commands are now available"
                     wait $ cmd "/cancel" (const cancel)
                         <> cmd "/test" send
                     wait $ cmd "/test2" send)
      <> cmd "/io" (\_ -> do
                     x <- liftIO $ T.pack . show <$> getZonedTime
                     send x)

fakeIn :: Producer Text IO ()
fakeIn = do
  liftIO $ putStr "Write something to the bot: "
  x <- liftIO $ T.pack <$> getLine
  if x == "q"
     then pure ()
     else yield x *> fakeIn

runBotInTerminal bot = do
  putStrLn "Starting bot."
  putStrLn "Press q at any time to quit"
  runEffect $ fakeIn >-> (forever $ mkPipesBot bot)
