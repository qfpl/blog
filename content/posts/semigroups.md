---
title: Please Depend on Semigroups
date: 2017-10-25
project: infra
authors: gwilson
---

## An Abridged History of Semigroup in Haskell

Monoids have been a ubiquitous part of modern functional programming for many
years. Recently Semigroups, which are more general but weaker than monoids,
have seen a lot of use as well.

At the time `Monoid` was added to base, a `Semigroup` class was not added
alongside it. In hindsight, we would have liked a `Semigroup` class as a
superclass of `Monoid`. Every Monoid is a Semigroup, and there are lots of
useful structures that are only semigroups, like `NonEmpty` lists. NonEmpty
Lists lack the empty list, which is the identity of the append operation.
Other useful structures come about that are related to Semigroup but don't
need a full Monoid, such as [`Validation`](https://github.com/qfpl/validation/)
and [`Foldable1`](https://hackage.haskell.org/package/semigroupoids/docs/Data-Semigroup-Foldable.html).
Hence Edward Kmett wrote a library called [semigroups](https://hackage.haskell.org/package/semigroups).
The package provides the `Semigroup` class and `NonEmpty` lists. Due to the
usefulness of the abstraction, and the fact that many of Edward's other
packages depend on it, the semigroups package is
[very widely used](https://packdeps.haskellers.com/reverse/semigroups).

Later, `Semigroup` and `NonEmpty` were added to `base` in version 4.9[^1], meaning
that anyone using GHC 8.0 or higher would get them for free, without any
external dependencies.

## Why your library should still depend on `semigroups`

Now that base has semigroups, it is tempting to remove our dependencies on
the semigroups package and deprecate it out of existence. But there is a
good reason to still depend on it: supporting older GHCs.

If you use the
`Semigroup` typeclass without depending on the semigroups package, your code
will only run on GHC >= 8.0. But at the time of writing, several
[popular](https://packages.ubuntu.com/search?keywords=ghc)
[operating](https://fedoraproject.org/wiki/Haskell_Platform)
[systems](https://software.opensuse.org/package/ghc)
are still shipping GHC 7.10, and there are probably plenty of people stuck on
older versions for their particular project or team. Adding a semigroups
dependency helps the community, because it can make the difference between your
library being usable for someone or unusable.

But looking at the module names in the semigroups package, it would seem they
will clash with the modules recently added to base! Thankfully this is not the
case, because semigroups is smart enough to "turn itself off" with CPP when it
detects a new enough base version.

In summary, please depend on semigroups if you depend on `Semigroup` :)

[^1]:
  In order to preserve backward compatibility, `Semigroup`
  is not currently a superclass of `Monoid`, but in a later version
  [it will be](https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid).

