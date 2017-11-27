---
title: Multi-Parameter Type Classes and their Orphan Rules
date: 2017-11-27
authors: gwilson
---

*If you are already familiar with multiparameter type classes, functional dependencies,
and orphan instances, you can skip to the final section for the orphan rules.*

## Type classes and orphan instances

Haskell's type classes are a powerful and practical means of ad-hoc polymorphism.
One aspect of this, the global coherence of type class instances, enables us to
reason about the behaviour of type classes effectively. If you're interested in
an in-depth exploration of that topic, I'd recommend
[this talk](https://www.youtube.com/watch?v=hIZxTQP1ifo).

One of the requirements for the coherence property is the absence of
*orphan instances*. An instance of a type class for a type is not
an orphan if it is defined in the same file as that type class, or
in the same module as that type. It is an orphan instance if it is defined in
any module other than these two. So for example, the `Eq` instance for `Int`
could be defined in the same module as `Eq` is defined (`GHC.Classes`), or in the
same module as `Int` is defined (`GHC.Types`). The instance would be an orphan
instance if it were defined in, for example, `Data.String`, because that module
neither defines `Eq` nor `Int`.

## Multi-Parameter Type classes and functional dependencies

Over the years. there have been many GHC language extensions extending the
type class mechanism in different ways. I recommend the 
[24 Days of GHC Extensions](https://ocharles.org.uk/blog/posts/2014-12-01-24-days-of-ghc-extensions.html)
blog series as an accessible introduction to several of them, among other
commonly-used GHC language extensions.
One such extension which pops up fairly often is *multi-parameter type classes*,
which is enabled with a language pragma like so:

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
```

Multiparameter type classes are quite useful, but they introduce ambiguity that
tends to cause problems with type inference, often requiring annotations in
inconvenient locations. To remedy this problem, the *functional dependencies*
language extension was added, allowing us to specify that one or several type
parameters uniquely determine some other type parameter. The type inferencer
can use this information to resolve the ambiguity and carry on with its work.

Here's an example of a multi-parameter type class with functional dependencies,
the `Measured` type from the
[fingertree package](https://hackage.haskell.org/package/fingertree)[^1].


```haskell
class Monoid v => Measured v a | a -> v where
  measure :: a -> v
```

This class says that values of type `a` are measured by some monoid `v`, whose
monoidal structure will let us combine these measurements. The functional
dependency `a -> v` on the first line tells us that the type `a` uniquely
determines the type `v`.

Here's an instance of `Measured` for lists.

```haskell
instance Measured (Sum Int) [x] where
  measure xs = Sum (length xs)
```

When the type inferencer learns what type `a` is (in this case `[x]`), it will
learn what `v` is (in this case `Sum Int`).
This greatly helps reduce the ambituity multi-parameter type classes introduce,
meaning GHC can infer the types of expressions like the following.

```haskell
measure ['a', 'b'. 'c'] <> measure ['d', 'e']
```

Without the functional dependency `a -> v` in the type class declaration,
that expression would not compile, as the intended measure type `v` would be
ambiguous in that expression.

But there is a penalty to functional dependencies. To continue with our `Measured`
example: we can't have several different measures for the same type.
This is precisely what functional dependencies enforce;
the type `a` must *uniquely* determine the choice of `v`. So we can't
make another instance with list in the `a` position. That would violate the
uniqueness.
We can still make as many instances with `Sum Int` in the `v` position as we'd
like, however, because `v` does *not* uniquely determine `a`. For example we
could make an instance that measures `Set`s using `Sum Int`:

```haskell
import qualified Data.Set as S

instance Measured (Sum Int) (S.Set a) where
  measure set = Sum (S.size set)
```

## Orphan Rules

I recently came up against an unexpected orphan instance warning in my code,
which caused me to ask the question eventually leading to this article:
When is an instance of a multi-parameter type class considered an orphan
instance?
After some digging, I found the answer [here](https://ghc.haskell.org/trac/ghc/ticket/11999#comment:1).

To summarise the above link: in order to not be an orphan, a multiparameter
typeclass instance involving functional dependencies can occur in the module
where the typeclass is defined, or in a module which defines a type that is *not
determined* by any of the functional dependencies.

For our `Measured` example above and its instance `Measured (Sum Int) [a]`,
that instance could occur in the module that defines `Measured`, or the module
that defines list. That instance would be an orphan if it
occured in the module where `Sum` is defined, since `Sum Int` is determined by
`[a]` according to the functional dependency.

This has a consequence that I've hit recently. Suppose you have something like:

```haskell
class Wibble a b | a -> b, b -> a where
  wobble :: a -> b
```

Here we have a pair of functional dependencies which together describe a
bidirectional relationship. In this case, where could our instance be?
Remember that `a` is determined by `b`, and `b` is determined by `a`. So
we can't put our instance in either of their modules according to the rules
above, unless they happen to be defined in the same module. Often they are not.
Hence the only place this instance can live is in the module that defines the
type class.

Another solution to this is to define two newtypes in the same module - one for
each of the types - and then give this instance to the newtypes.

[^1]:
Finger trees are an interesting concept on their own, so you might like to read
about them
[here](http://www.staff.city.ac.uk/~ross/papers/FingerTree.html).)

