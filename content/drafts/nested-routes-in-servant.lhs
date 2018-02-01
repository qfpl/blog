---
title: Nested routes in servant
date: 2018-02-01
authors: ajmccluskey
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
tthen come back.

This post is literate haskell, so feel free to grab [the
code](https://github.com/qfpl/blog/tree/master/content/posts/nested-routes-in-servant.lhs) and play
along at home. If you're running nix you can use `nix-shell -p 'haskellPackages.ghcWithPackages (hp:
 [hp.servant])'` to get a shell with everything you need. From there you can fire up `ghci` and load
the file.

<h3>Setup</h3>

We'll start by importing what we need from `servant` and enabling the language extensions that
`servant` needs.

\begin{code}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Proxy               (Proxy (Proxy))
import           Data.Text                (Text, pack)
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp (run)
import           Servant                  ((:<|>) ((:<|>)), (:>), Capture,
                                           FromHttpApiData (parseUrlPiece), Get,
                                           PlainText, Server, Handler, serve)
\end{code}

<h3>A small server</h3>

Now we'll define our first server. We'll start small and simple, without any nesting, and build up
from there.

\begin{code}
type ApiWithDuplication =
       "first-bit" :> "second-bit" :> Get '[PlainText] Text
  :<|> "first-bit" :> "second-bit" :> "life" :> Get '[PlainText] Text
  :<|> "first-bit" :> "second-bit" :> "random" :> Get '[PlainText] Text

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
  :: Handler Text
life =
  renderShowable 42

random
  :: Handler Text
random =
  -- chosen by fair dice roll
  renderShowable 4

renderShowable
  :: Show a
  => a
  -> Handler Text
renderShowable =
  return . pack . show

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
  :<|> "life" :> Get '[PlainText] Text
  :<|> "random" :> Get '[PlainText] Text
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

All we had to do in this case was factor out the common prefix of each route by only specifying it
once, and then joining that to the combined sub-routes with the common prefix.

Notice that our server definition hasn't changed. That's because the type of our server hasn't
changed. `Server` is a type family (think function at the type level) that, given our two different
API types, produces the same type. That is to say that `Server ApiWithDuplication` is equal to
`Server ApiWithoutDuplication` in the same way that `3 - 1 == 1 + 1`.

To prove that I'm not lying about the types, let's ask our good friend `ghci`.

```
λ> :t apiWithDuplicationServer
apiWithDuplicationServer
  :: Handler [Char] :<|> (Handler [Char] :<|> Handler [Char])

λ> :t apiWithoutDuplicationServer
apiWithoutDuplicationServer
  :: Handler [Char] :<|> (Handler [Char] :<|> Handler [Char])
```

In turning our API type into the type of the handler, we have arrived at the same type. The
intuition is that the common part of our routes is static, and doesn't impact the type of the
ifunctions used to handle those requests. As a result, we end up with a chain of handler functions
with an identical type.

If you still don't believe me, scroll up and look carefully at `runApiWithoutDuplication`. We're not
even using the second server we defined, we're using the original: `apiWithDuplicationServer`.

<h3>Something variable this way comes</h3>

You might now be asking what happens if I have nested routes where the common elements contain
variables that our handlers need to capture. At least, I hope you are, because if not this next
section is really going to disappoint you.

Let's start by concocting a route with a common `Capture` and duplication.

\begin{code}
data Adventurer =
  Adventurer
  { adventurerKlass :: Text
  , adventurerActor :: Text
  } deriving (Show)

dm, magnus, merle, taako :: Adventurer

dm     = Adventurer "DM"             "Griffin"
magnus = Adventurer "Human Fighter"  "Travis"
merle  = Adventurer "Dwarven Cleric" "Clint"
taako  = Adventurer "Elven Wizard"   "Justin"

instance FromHttpApiData Adventurer where
  parseUrlPiece a =
    case a of
      "dm"     -> Right dm
      "magnus" -> Right magnus
      "merle"  -> Right merle
      "taako"  -> Right taako
      _        -> Left "Unknown adventurer"

type TazApiDup =
       "adventurer" :> Capture "adventurer" Adventurer :>
         "class" :> Get '[PlainText] Text
  :<|> "adventurer" :> Capture "adventurer" Adventurer :>
         "actor" :> Get '[PlainText] Text

tazApiDupServer
  :: Server TazApiDup
tazApiDupServer =
  klass :<|> actor

klass, actor
  :: Adventurer
  -> Handler Text

klass = return . adventurerKlass
actor = return . adventurerActor

runTazApiDup :: IO ()
runTazApiDup =
  run 8081 . serve (Proxy :: Proxy TazApiDup) $ tazApiDupServer
\end{code}

There's some obvious duplication here, so let's factor it out again.

\begin{code}
type TazApi =
  "adventurer" :> Capture "adventurer" Adventurer :>
  (    "class" :> Get '[PlainText] Text
  :<|> "actor" :> Get '[PlainText] Text
  )
\end{code}

Much better! But what happens if we try to use our old server?

```haskell
-- This code isn't part of the literate haskell
tazApiServer
  :: Server TazApi
tazApiServer =
  klass :<|> actor

{-
Couldn't match type ‘(Adventurer -> Handler Text)
                           :<|> (Adventurer -> Handler Text)’
                     with ‘Adventurer -> Handler Text :<|> Handler Text’
      Expected type: Server TazApi
        Actual type: (Adventurer -> Handler Text)
                     :<|> (Adventurer -> Handler Text)
-}
```

Our friendly compiler who is our friend has told us we've made an error. Specifically, it's telling
us that `Server TazApi` is a synonym for `Adventurer -> Handler Text :<|> Handler Text`, but we've
provided a definition with type `(Adventurer -> Handler Text) :<|> (Adventurer -> Handler Text)`. To
understand why, let's back up a step.

TODO:

- Captures/variables result in function arguments via `Server` type family
- We joined together two routes that each took a capture in the duplicated version,
  so we needed to join together two functions that each took an adventurer
- De-duping the API resulted in a server type that takes an adventurer and returns two sub-routes,
  so we need to change the definition of our server to reflect this
- Run through handling this change for servant clients
