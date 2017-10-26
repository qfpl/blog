---
title: Optimising free monad programs using Plated
date: 2017-10-26
authors: ielliott
---

This document is a Literate Haskell file, available [here](https://github.com/qfpl/blog/blob/master/content/posts/optimising-free-with-plated.lhs)

---

In this article I demonstrate how to use classy prisms and `Plated` to write and
apply optimisations to programs written in a free monad DSL.

[Plated](https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Plated.html#t:Plated)
is a class in [lens](https://hackage.haskell.org/package/lens) that provides
powerful tools to work with self-recursive data structures. One such tool is
[recursive bottom-up rewriting](https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Plated.html#v:rewrite),
which repeatedly applies a transformation everywhere in a `Plated` structure
until it can no longer be applied.

[The free monad](https://hackage.haskell.org/package/free-4.12.4/docs/Control-Monad-Free.html#t:Free)
has an instance of `Plated`:

```haskell
Traversable f => Plated (Free f a)
```
so if you derive `Foldable` and `Traversable` for the underlying functor,
you can use the `Plated` combinators on your free monad DSL.

Defining [classy prisms](https://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-TH.html#v:makeClassyPrisms)
for the base functor provides pattern matching on free monad programs for free.
When coupled with `rewrite`, we get a system for applying optimisations to
our free monad programs with minimal effort.

---

Let's get into it.

\begin{code}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}

module FreePlated where

import Control.Lens
import Control.Monad.Free
import Data.Monoid
\end{code}

&nbsp;

First, define the DSL. This is a bit of a contrived one:

\begin{code}
data InstF a
  = One a
  | Many Int a
  | Pause a
  deriving (Functor, Foldable, Traversable, Eq, Show)

type Inst = Free InstF

one :: Inst ()
one = liftF $ One ()

many :: Int -> Inst ()
many n = liftF $ Many n ()

pause :: Inst ()
pause = liftF $ Pause ()
\end{code}

* `one` - "do something once"
* `many n` - "do something `n` times"
* `pause` - "take a break"

In this DSL, we are going impose the property that `many n` should be
equivalent to `replicateM_ n one`.

&nbsp;

Next, generate classy prisms for the functor.

\begin{code}
makeClassyPrisms ''InstF
\end{code}

`makeClassyPrisms ''InstF` generates the following prisms:

*
    ```haskell
    _InstF :: AsInstF s a => Prism' s (InstF a)`
    ```
*
    ```haskell
    _One :: AsInstF s a => Prism' s a
    ```
*
    ```haskell
    _Many :: AsInstF s a => Prism' s (Int, a)`
    ```
*
    ```haskell
    _Pause :: AsInstF s a => Prism' s a`
    ```

&nbsp;

Lift the classy prisms into the free monad:

\begin{code}
instance AsInstF (Inst a) (Inst a) where
  _InstF = _Free
\end{code}

We can now use the prisms as if they had these types:

*
    ```haskell
    _One :: Prism' (Inst a) (Inst a)
    ```
*
    ```haskell
    _Many :: Prism' (Inst a) (Int, Inst a)
    ```
*
    ```haskell
    _Pause :: Prism' (Inst a) (Inst a)
    ```

If one of these prisms match, it means the program begins with that particular
instruction, and the `Inst a` returned is the tail of the program.

&nbsp;

Now it's time to write optimisations over the free monad structure. A
rewrite rule has the type `a -> Maybe a`- if the function returns a `Just`,
the input will be replaced with the contents of the `Just`. If it returns
`Nothing` then no rewriting will occur.

\begin{code}
optimisations :: AsInstF s s => [s -> Maybe s]
optimisations = [onesToMany, oneAndMany, manyAndOne]
  where
\end{code}

Rule 1: `one` followed by `one` is equivalent to `many 2`

\begin{code}
    onesToMany s = do
      s' <- s ^? _One._One
      pure $ _Many # (2, s')
\end{code}

Rule 2: `one` followed by `many n` is equivalent to `many (n+1)`

\begin{code}
    oneAndMany s = do
      (n, s') <- s ^? _One._Many
      pure $ _Many # (n+1, s')
\end{code}

Rule 3: `many n` followed by `one` is equivalent to `many (n+1)`

\begin{code}
    manyAndOne s = do
      (n, s') <- s ^? _Many.aside _One
      pure $ _Many # (n+1, s')
\end{code}

&nbsp;

The last step is to write a function that applies all the optmisations to
a program.

\begin{code}
optimise :: (Plated s, AsInstF s s) => s -> s
optimise = rewrite $ getFirst . foldMap (First .) optimisations
\end{code}

`getFirst . foldMap (First .)` has type `[a -> Maybe a] -> a -> Maybe a`.
It combines all the rewrite rules into a single rule that picks the first
rule to succeed for the input.

&nbsp;

Now we can optimise a program:

\begin{code}
program = do
  one
  one
  one
  pause
  one
  one
  many 3
  one
\end{code}

The `one`s before the `pause` should collapse into `many 3`, and the
instructions after the `pause` should collapse into `many 6`.

```
ghci> optimise program == (many 3 *> pause *> many 6)
True
```

:)
