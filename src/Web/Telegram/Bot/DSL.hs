module Web.Telegram.Bot.DSL
  ( send
  , cmd
  , choice
  , input
  , mkBot
  , Bot
  , BotM
  , BotCommand
  , runBotInTerminal
  , (<>)
  , MonadIO
  , liftIO ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup ((<>))
import Web.Telegram.Bot.Internal
