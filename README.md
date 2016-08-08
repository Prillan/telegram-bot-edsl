This is a WIP. You have been warned.

## Telegram Bot EDSL

This is a library for easy creation of telegram bots. A full example
can be found in `example/Main.hs`.

Note: Currently requires
[this patched version of `telegram-api`](https://github.com/Prillan/haskell-telegram-api/tree/setwebhook-patch). It
adds the possibility of uploading self-signed certs.

## Syntax

The DSL is built on top of
[`pipes`](https://www.stackage.org/package/pipes) using `await` to get
an update from telegram and `yield` to send a response back. It also
provides a set of combinator to make all of this easier.

### Base

```haskell
type Bot m = Proxy () BotInput () BotOutput (ReaderT BotEnv m)

type MessageId = Int
data BotInput = BotTextMessage { botTextMessageText   :: Text
                               , botTextMessageId     :: MessageId
                               , botTextMessageUserId :: Int }

data BotOutput = BotSend Text (Maybe MessageId)
data BotEnv = BotEnv { botName :: Text }
```

### Combinators

```haskell
send           :: Monad m => Text -> Bot m ()
input          :: Monad m => Bot m Text
inputWithReply :: Monad m => Bot m (Text, Text -> Bot m ())
env            :: Monad m => Bot m BotEnv
cmd            :: Monad m => Text -> (Text -> Bot m a) -> BotCommand (Bot m a)
choice         :: Monad m => BotCommand (Bot m a) -> Bot m a
```

### Example
```haskell
import qualified Data.Text as T
import Data.Time (getZonedTime)
import Web.Telegram.Bot.DSL

bot = choice $ cmd "echo"    (\m -> send m)
            <> cmd "reverse" (\m -> send (T.reverse m))
            <> cmd "date"    (\_ -> do
                               t <- liftIO $ getZonedTime
                               send $ "The time is: " <> (T.pack . show $ t))
```

## How to run it

You can always test your bot in the terminal by running
`runBotInTerminal bot`.

There are two main ways of running the bot, via webhooks or long
polling.

### Run the bot with webhooks

You need five things.

1. The ssl certificate file.
2. The ssl private key file.
3. A url that telegram will send it's updates too. (Needs to match the
   ssl certificate)
4. The local port
5. The bot token

#### Generating a SSL certificate

See
[Generating a self-signed certificate pair](https://core.telegram.org/bots/self-signed).

#### Getting a bot token

Follow the steps over at
[Bots: An introduction for developers](https://core.telegram.org/bots#6-botfather).

#### What's up with the remote url and local port?

In order for your bot to receive update you have to expose it to the
internet. The first step is to get a domain pointing at your server,
let's say `example.com`. Then you need an open port to point at your
local server port. Telegram will only send updates to the ports 80,
88, 443 and 8443 so make sure that you're using one of those.

Example:
`example.com  → x.y.z.w`
`:8443 → :5000`

#### (Optional) Extra server settings

See [the docs for the Warp web server](https://www.stackage.org/package/warp).

#### Putting it all together

```haskell
bs = BotSettings
  {
    bsSslCert        = "<PATH_TO_SELF_SIGNED_CERT>.pem"
  , bsSSlKey         = "<PATH_TO_SELF_SIGNED_KEY>.key"
  , bsRemoteUrl      = "https://example.com:8443"
  , bsListeningPort  = 5000
  , bsToken          = "<YOUR_BOT_TOKEN>"
  , bsServerSettings = Nothing
  , bsBotEnv         = BotEnv "<BOT_USER_NAME>"
  }
```

Run the server: `runBotWithWebhooks bs bot`.


### Run the bot with long polling

This is a lot easier and you only need the bot token and the bot user
name.

```haskell
env = BotEnv
  {
    botName = "<BOT_USER_NAME>"
  }
token = "<BOT_TOKEN>"
```

Run it: `runBotWithPolling token env`
