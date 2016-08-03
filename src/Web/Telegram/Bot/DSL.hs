{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Telegram.Bot.DSL
  ( Action(..)
  , Bot(..)
  , Listener(..)
  , MkBot(..)
  , (<+>)
  , msg
  , cmd ) where

import Data.Maybe (maybeToList)
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import qualified Data.Text as T
import Web.Telegram.API.Bot ( Update(..)
                            , Message(..) )


--data Target = Chat Int | User Text
data Action = Reply Text
--            | Msg Target Text
            | NoOp
  deriving Show

newtype Listener = Listener { runListener :: Update -> [Action] }
newtype Bot = Bot { listeners :: [Listener] }
  deriving (Semigroup, Monoid)

class MkBot a where
  mkBot :: a -> Bot

instance MkBot Listener where
  mkBot = Bot . pure
instance MkBot Bot where
  mkBot = id

(<+>) :: (MkBot a, MkBot b) => a -> b -> Bot
x <+> y = mkBot x <> mkBot y

newMessage :: (Text -> [Action]) -> Listener
newMessage f = Listener $ \u ->
  case (text =<< message u) of
    Just m  -> f m
    Nothing -> []

msg :: (Text -> [Action]) -> Listener
msg = newMessage

cmd :: Text -> (Text -> Maybe Text) -> Listener
cmd prefix f =
  newMessage $ \m ->
    if prefix `T.isPrefixOf` m
       then maybeToList (Reply <$> f (T.drop (T.length prefix + 1) m))
       else []
