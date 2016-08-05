This is a WIP. You have been warned.

## Telegram Bot EDSL

This is a library for easy creation of telegram bots. A full example
can be found in `example/Main.hs`.

Note: Currently requires
[this patched version of `telegram-api`](https://github.com/Prillan/haskell-telegram-api/tree/setwebhook-patch). It
adds the possibility of uploading self-signed certs.

## Syntax

The DSL is built on top of `FreeT`, providing you with all your
favorite `IO` actions at the bottom of a safe layer. This layer is the
monad `BotM m` for some base monad `m`.

### Primitives

```haskell
cmd    :: Monad m => Text -> (Text -> BotM m a) -> BotCommand (BotM m a)
choice :: Monad m => BotCommand (BotM m a) -> BotM m a
input  :: Monad m => BotM m Text
send   :: Monad m => Text -> BotM m ()
```

The type `BotCommand a` is just a synonym for `[(Text, Text -> a)]` so
everything that works on lists work on `BotCommand` as well. It comes
in handy when selecting between different commands, see the example
below.

### Example
```haskell
import qualified Data.Text as T
import Data.Time (getZonedTime)
import Web.Telegram.Bot.DSL

bot = choice $ cmd "/echo"    (\m -> send m)
            <> cmd "/reverse" (\m -> send (T.reverse m))
            <> cmd "/help"    (\_ -> send "This is the help message!")
            <> cmd "/date"    (\_ -> do
                                t <- liftIO $ getZonedTime
                                send $ "The time is: " <> (T.pack . show $ t))
```

## How to run it

You can always test your bot in the terminal by running
`runBotInTerminal bot`. In order to put it live, you need five things.

1. The ssl certificate file.
2. The ssl private key file.
3. A url that telegram will send it's updates too. (Needs to match the
   ssl certificate)
4. The local port
5. The bot token

### Generating a SSL certificate

See
[Generating a self-signed certificate pair](https://core.telegram.org/bots/self-signed).

### Getting a bot token

Follow the steps over at
[Bots: An introduction for developers](https://core.telegram.org/bots#6-botfather).

### What's up with the remote url and local port?

In order for your bot to receive update you have to expose it to the
internet. The first step is to get a domain pointing at your server,
let's say `example.com`. Then you need an open port to point at your
local server port. Telegram will only send updates to the ports 80,
88, 443 and 8443 so make sure that you're using one of those.

Example:
`example.com  → x.y.z.w`
`:8443 → :5000`

### (Optional) Extra server settings

See [the docs for the Warp web server](https://www.stackage.org/package/warp).

### Putting it all together

```haskell
bs = BotSettings
  {
    bsSslCert        = "<PATH_TO_SELF_SIGNED_CERT>.pem"
  , bsSSlKey         = "<PATH_TO_SELF_SIGNED_KEY>.key"
  , bsRemoteUrl      = "https://example.com:8443"
  , bsListeningPort  = 5000
  , bsToken          = "<YOUR_BOT_TOKEN>"
  , bsServerSettings = Nothing
  }
```

Run the server: `runBot bs bot`.
