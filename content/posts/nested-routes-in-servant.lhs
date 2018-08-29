---
title: Nested routes in servant
date: 2018-02-05
authors: ajmcmiddlin
---

One of the first Haskell apps I ever wrote was a servant app. It had three routes and I cargo culted
the setup from a more knowledgeable colleague, but I had a lot of fun. As a relative newcomer to
Haskell I was surprised at how accessible the documentation was given the library uses some advanced
type-level machinery to achieve its goals. However, one stumbling block I had was trying to
factor out some common parts from the routes. I've spoken to a few people about this, and they've
had similar troubles. Over the last few days I've come back to servant for a work project and hit
the problem again. This time I cracked it. Here are the fruits of my labour.

I'm going to assume that you're already familiar with the basics of servant. If not, go check out
their [excellent documentation](https://haskell-servant.readthedocs.io/en/stable/index.html) and
then come back.

This post is literate haskell, so feel free to grab [the
code](https://github.com/qfpl/blog/tree/master/content/posts/nested-routes-in-servant.lhs) and play
along at home. If you're running nix you can use `nix-shell -p 'haskellPackages.ghcWithPackages (hp:
[hp.servant-client hp.servant-server])'` to get a shell with everything you need. From there you can
fire up `ghci` and load the file.

<h3>Setup</h3>

We'll start by importing what we need from servant and enabling some language extensions.

\begin{code}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Monad.Error.Class (throwError)
import qualified Data.Map as M
import           Data.Proxy               (Proxy (Proxy))
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant                  ((:<|>) ((:<|>)), (:>), Capture,
                                           FromHttpApiData (parseUrlPiece), Get,
                                           Handler, JSON, PlainText, Server,
                                           ToHttpApiData (toUrlPiece), err404, serve)
import           Servant.Client           (BaseUrl (BaseUrl), ClientEnv (ClientEnv),
                                           ClientM, Scheme (Http), ServantError,
                                           client, runClientM)
import           Web.HttpApiData          (parseBoundedTextData, showTextData)
\end{code}

<h3>A small server</h3>

Now we'll define our first server. We'll start small and simple, without any nesting, and build up
from there.

\begin{code}
type ApiWithDuplication =
       "first-bit" :> "second-bit" :> Get '[PlainText] Text
  :<|> "first-bit" :> "second-bit" :> "life" :> Get '[JSON] Int
  :<|> "first-bit" :> "second-bit" :> "random" :> Get '[JSON] Int

apiWithDuplicationServer
  :: Server ApiWithDuplication
apiWithDuplicationServer =
       secondBit
  :<|> life
  :<|> random

secondBit
  :: Handler Text
secondBit =
  return "I'm the root of second-bit"

life
  :: Handler Int
life =
  return 42

random
  :: Handler Int
random =
  -- chosen by fair dice roll
  return 4

runApiWithDuplication :: IO ()
runApiWithDuplication =
  run 8081 . serve (Proxy :: Proxy ApiWithDuplication) $ apiWithDuplicationServer
\end{code}

We have three routes here, all with a common prefix. Our server (`apiWithDuplicationServer`) mirrors
the structure of our API using the term-level version of `(:<|>)` to join our handlers in the
correct order.

<h3>Remove the duplication</h3>

Now, let's remove some duplication.

\begin{code}
type ApiWithoutDuplication =
  "first-bit" :> "second-bit" :>
  (    Get '[PlainText] Text
  :<|> "life" :> Get '[JSON] Int
  :<|> "random" :> Get '[JSON] Int
  )

apiWithoutDuplicationServer
  :: Server ApiWithoutDuplication
apiWithoutDuplicationServer =
       secondBit
  :<|> life
  :<|> random

runApiWithoutDuplication :: IO ()
runApiWithoutDuplication =
  run 8081 . serve (Proxy :: Proxy ApiWithoutDuplication) $ apiWithDuplicationServer
\end{code}

Now our API only specifies the common part of the routes once, followed by the sub-routes.

Notice that our server definition hasn't changed. That's because the type of our server hasn't
changed. `Server` is a type family (think function at the type level) that, given our two different
API types, produces the same type. That is to say that `Server ApiWithDuplication` is equal to
`Server ApiWithoutDuplication` in the same way that `3 - 1 == 4 - 2`. The intuition is that the
common part of our routes is static, and doesn't impact the type of the functions used to handle
those requests. As a result, we end up with a chain of handler functions with an identical type.

To prove that I'm not lying about the types, let's ask our good friend `ghci`.

```
λ> :t apiWithDuplicationServer
apiWithDuplicationServer
  :: Handler [Char] :<|> (Handler [Char] :<|> Handler [Char])

λ> :t apiWithoutDuplicationServer
apiWithoutDuplicationServer
  :: Handler [Char] :<|> (Handler [Char] :<|> Handler [Char])
```

If you still don't believe me, scroll up and look carefully at `runApiWithoutDuplication`. We're not
even using the second server we defined, we're using the original: `apiWithDuplicationServer`.

<h3>Something variable this way comes</h3>

You might now be asking what happens if we have nested routes where the common elements contain
variables that our handlers need to capture. At least, I hope you are, because if not this next
section is really going to disappoint you.

Let's start by concocting a route with a common `Capture` and duplication.

\begin{code}
data Adventurer =
  Adventurer
  { adventurerKlass :: Text
  , adventurerActor :: Text
  , adventurerStats :: M.Map Stat Int
  } deriving Show

data TazAdventurer
  = Magnus
  | Merle
  | Taako
  deriving (Bounded, Enum, Read, Show)

data Stat
  = HP
  | AC
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromHttpApiData TazAdventurer where
  parseUrlPiece = parseBoundedTextData

instance FromHttpApiData Stat where
  parseUrlPiece = parseBoundedTextData

fromTaz
  :: TazAdventurer
  -> Adventurer
fromTaz ta =
  case ta of
    Magnus -> Adventurer "Human Fighter"  "Travis" (M.fromList [(HP, 112), (AC, 19)])
    Merle  -> Adventurer "Dwarven Cleric" "Clint"  (M.fromList [(HP, 65), (AC, 14)])
    Taako  -> Adventurer "Elven Wizard"   "Justin" (M.fromList [(HP, 56), (AC, 13)])

type TazApiDup =
       "adventurer" :> Capture "tazAdventurer" TazAdventurer :>
         "class" :> Get '[PlainText] Text
  :<|> "adventurer" :> Capture "tazAdventurer" TazAdventurer :>
         "actor" :> Get '[PlainText] Text
  :<|> "adventurer" :> Capture "tazAdventurer" TazAdventurer :>
         "stats" :> Capture "stat" Stat :> Get '[JSON] Int

tazApiDupServer
  :: Server TazApiDup
tazApiDupServer =
  klass :<|> actor :<|> stat

klass, actor
  :: TazAdventurer
  -> Handler Text

klass = return . adventurerKlass . fromTaz
actor = return . adventurerActor . fromTaz

stat
  :: TazAdventurer
  -> Stat
  -> Handler Int
stat ta s =
  let
    ms = M.lookup s . adventurerStats . fromTaz $ ta
  in
    maybe (throwError err404) return ms

runTazApiDup :: IO ()
runTazApiDup =
  run 8081 . serve (Proxy :: Proxy TazApiDup) $ tazApiDupServer
\end{code}

There's some obvious duplication here, so let's factor it out.

\begin{code}
type TazApi =
  "adventurer" :> Capture "tazAdventurer" TazAdventurer :>
  (    "class" :> Get '[PlainText] Text
  :<|> "actor" :> Get '[PlainText] Text
  :<|> "stats" :> Capture "stat" Stat :> Get '[JSON] Int
  )
\end{code}

Much better! But what happens if we try to use our old server?

```haskell
-- This code isn't part of the literate haskell
tazApiServer
  :: Server TazApi
tazApiServer =
  klass :<|> actor :<|> stat

{-
Couldn't match type ‘(TazAdventurer -> Handler Text)
                           :<|> ((TazAdventurer -> Handler Text)
                                 :<|> (TazAdventurer -> Stat -> Handler Int))’
                     with ‘TazAdventurer
                           -> Handler Text :<|> (Handler Text :<|> (Stat -> Handler Int))’
      Expected type: Server TazApi
        Actual type: (TazAdventurer -> Handler Text)
                     :<|> ((TazAdventurer -> Handler Text)
                           :<|> (TazAdventurer -> Stat -> Handler Int))
-}
```

Our friendly compiler has told us we've made an error. Specifically, it's telling us that `Server
TazApi` is a synonym for `TazAdventurer -> Handler Text :<|> Handler Text :<|> (Stat -> Handler
Int)`, but we've provided a definition with type `(TazAdventurer -> Handler Text) :<|>
(TazAdventurer -> Handler Text) :<|> (TazAdventurer -> (Stat -> Handler Int))`.

As mentioned earlier, `Server` is a type family that, given the type of an API, produces the type of
the server required to handle that API. The types of the handler functions produced include any
inputs, such as captures or the request body, as function arguments. _This_ is why `Server
TazApiDup` isn't equal to `Server TazApi` - the former expects three functions that each take a
`TazAdventurer` as an argument, while the latter has factored out the common capture and expects a
function from `TazAdventurer` to the handlers for the remaining parts of the routes.

Knowing all this, the solution hopefully makes sense: we need to provide a server definition that
matches the generated type. That is, a server that takes the `TazAdventurer` as an argument, and
then distributes it over each sub-route so that the type of each partially applied function matches
the type of the server.

\begin{code}
tazApiServer
  :: Server TazApi
tazApiServer a =
  klass a :<|> actor a :<|> stat a

runTazApi
  :: IO ()
runTazApi =
  run 8081 . serve (Proxy :: Proxy TazApi) $ tazApiServer
\end{code}

<h3>The client side</h3>

One of the great things about servant is that because it represents an API as a type, it can use
that type to produce both servers and clients for the API. So what happens if we want a client for a
nested API? Let's start by creating a client for `TazApiDup` to see how clients are made.

\begin{code}
instance ToHttpApiData TazAdventurer where
  toUrlPiece = showTextData

instance ToHttpApiData Stat where
  toUrlPiece = showTextData

classClient :<|> actorlClient :<|> statClient =
  client (Proxy :: Proxy TazApiDup)
\end{code}

Other than defining a couple of instances that allow servant to turn our `TazAdventurer` arguments
into parts of a URL, all we need to do is call `client` on our existing API and pattern match out
the client functions.

If we try to do the same thing with the nested API, we run into a problem similar to the one we
encountered when defining our server &mdash; the type of the nested API no longer lines up with our
pattern match on the client functions. Once again, this becomes clearer when we look at the types of
each generated client in ghci.

```
λ> :t client (Proxy :: Proxy TazApiDup)
client (Proxy :: Proxy TazApiDup)
  :: (TazAdventurer -> ClientM Text)
     :<|> ((TazAdventurer -> ClientM Text)
           :<|> (TazAdventurer -> Stat -> ClientM Int))
λ> :t client (Proxy :: Proxy TazApi)
client (Proxy :: Proxy TazApi)
  :: TazAdventurer
     -> ClientM Text :<|> (ClientM Text :<|> (Stat -> ClientM Int))
```

As we can see, `client (Proxy :: Proxy TazApi)` returns a function from `TazAdventurer` to our three
client functions. We can't pattern match on each route now, but we _can_ apply this function to a
`TazAdventurer` to get the client functions for that adventurer. To make things easier on our users,
especially when we have more deeply nested APIs, we can put our client functions in a record. We'll
use the `RecordWildcards` extension to save ourselves some boilerplate too.

\begin{code}
data TazApiClient
  = TazApiClient
  { tazClientClass :: ClientM Text
  , tazClientActor :: ClientM Text
  , tazClientStat  :: Stat -> ClientM Int
  }

mkTazApiClient
  :: TazAdventurer
  -> TazApiClient
mkTazApiClient ta =
  let
    tazClientClass
      :<|> tazClientActor
      :<|> tazClientStat
      = client (Proxy :: Proxy TazApi) ta
  in
    TazApiClient{..}

clientEnv
  :: IO ClientEnv
clientEnv = do
  let
    baseUrl = BaseUrl Http "localhost" 8081 ""
  manager <- newManager defaultManagerSettings
  pure $ ClientEnv manager baseUrl

runTazClient
  :: ClientM a
  -> IO (Either ServantError a)
runTazClient =
  (clientEnv >>=) . runClientM

tazAdventurerStat
  :: TazAdventurer
  -> Stat
  -> IO (Either ServantError Int)
tazAdventurerStat ta s =
    runTazClient . ($ s) . tazClientStat . mkTazApiClient $ ta
\end{code}

<h3>References</h3>

- [Servant docs](https://haskell-servant.readthedocs.io/en/stable/index.html)
- [Source of clients in a record](https://github.com/haskell-servant/servant/issues/335#issuecomment-172300487)

