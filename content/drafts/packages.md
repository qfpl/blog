---
title: Our Approach to Haskell Dependencies
date: 2017-08-11
authors: gwilson
---

It is no secret that we at the Queensland Functional Programming lab love
Haskell. In particular we are interested in writing libraries for others
to depend on. Primarily, Haskell packages come from
[Hackage](https://hackage.haskell.org/) and are managed with
[cabal](https://www.haskell.org/cabal/)[^1]

[^1]: For our own work, we mostly use cabal with [Nix](/posts/nix/introducing-nix).

This brings us to the question of how to specify our packages' version numbers
and our own dependencies in such a way that a user is likely to have a good day.
This article gives a brief overview of our approach.

# Glasgow Haskell Compiler Versions
Firstly we make sure our packages support the most recent stable major release
of GHC, as that is usually what we are developing against. At the time of
writing that is version 8.2.1.

Since not everyone upgrades immediately, we also try our best to support the
second-most recent major version, which is 8.0.2 at the time of writing.

When possibly, we also try to support older major versions of GHC shipped by
more slowly-moving Linux distributions: examples include
[Ubuntu](https://packages.ubuntu.com/search?keywords=ghc)
or [OpenSUSE](https://software.opensuse.org/package/ghc).
At the time of writing that means we're supporting at least as far back as
version 7.10.3

If you are using an older major version of GHC than is listed here and you'd
like to use our software, please get
in [contact](/contact) with us, or open an issue on our
[GitHub](https://github.com/qfpl)
We can see what we can do to get the code working on your GHC.

# Version Bounds
Our libraries tend to depend on many other libraries, which are managed using
cabal and its version bounds system.

The [Package Version Policy](https://pvp.haskell.org/) explains how version
numbers of Haskell packages work and how bounds should be specified. In
particular, the PVP says that both lower and upper bounds shall be
specified for every dependency.
It is our experience that packages which comply with this version bound policy
tend to have a better rate of older versions working. Particularly the lack of
upper bounds can lead to cabal selecting bad install plans in old versions of
the package, or with new versions of GHC.

### Determining appropriate lower bounds
TODO

### Determining appropriate upper bounds
TODO

## Tools we find useful

### TravisCI
TODO

### Hackage Build Matrix
TODO

### Hackage Metadata Revisions
TODO

