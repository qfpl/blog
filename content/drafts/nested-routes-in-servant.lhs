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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Proxy               (Proxy (Proxy))
import           Data.Text                (Text)
import           Network.Wai.Handler.Warp (run)
import           Servant                  ((:<|>) ((:<|>)), (:>), Get, PlainText, Server, Handler,
                                           serve)
\end{code}

<h3>A small server</h3>

Now we'll define our first server. We'll start small and simple, without any nesting, and build up
from there.

\begin{code}
type ApiWithDuplication =
       "first-bit" :> "second-bit" :> Get '[PlainText] String
  :<|> "first-bit" :> "second-bit" :> "life" :> Get '[PlainText] String
  :<|> "first-bit" :> "second-bit" :> "random" :> Get '[PlainText] String

apiWithDuplicationServer
  :: Server ApiWithDuplication
apiWithDuplicationServer =
       secondBit
  :<|> life
  :<|> random

secondBit
  :: Handler String
secondBit =
  return "I'm the root of second-bit"

life
  :: Handler String
life =
  (return . show) 42

random
  :: Handler String
random =
  -- chosen by fair dice roll
  (return . show) 4

runApiWithDuplication :: IO ()
runApiWithDuplication =
  run 8081 . serve (Proxy :: Proxy ApiWithDuplication) $ apiWithDuplicationServer
\end{code}

We have three routes here, all with a common prefix. Our server (`apiWithDuplicationServer`) mirrors
the structure of our API using the term-level version of `(:<|>)` to join our handlers in the
correct order.
