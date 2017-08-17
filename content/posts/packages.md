---
title: Our Approach to Haskell Dependencies
project: infra
date: 2017-08-17
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
tend to have a better track record for installing correctly. Particularly the
lack of upper bounds can lead to cabal selecting bad install plans in old
versions of the package, or with new versions of GHC.

### Determining appropriate lower bounds
To determine appropriate lower bounds for a particular package, we check the
changelog of the package in question and assess this based on what features we
need. Often we will not need the entire power of the library, so we can go
a few major versions back.

We also check the [Hackage matrix builder](https://matrix.hackage.haskell.org)
and avoid versions which have red squares, or don't support GHC versions we're
targeting.

### Determining appropriate upper bounds
Determining the correct upper bound is more difficult. Recall that the first
two components of a version number as specified by the PVP together form the
major version. So a major version looks like 4.12 rather than 4.

To be on the safe side, we often select the major version number immediately
after the major version you are using.

A more flexible approach is
to put the bound on the first segment of the version only. An example of an
upper bound like this is `base >= 4.7 && < 5`. This kind of dependency seems
common among more experienced Haskellers for certain libraries, such as base.
It cannot be recommended in general because a change in the second segment of a
version can still spell disaster for an unsuspecting project.

If we are not sure what to do with your bounds, we often look at the libraries
of well-regarded Haskellers, particularly if they depend on the package we're
interested in. The [lens](https://github.com/ekmett/lens)
library, for example, has a lot of dependencies so it has a lot of bounds from
which to draw inspiration.

## Tools we find useful

### TravisCI
[Travis CI](https://travis-ci.org/) is a useful continuous integration tool
that's free for open source projects. We use
[this script](https://github.com/hvr/multi-ghc-travis) to generate a sensible
default Travis file for all the GHC versions we care about. It seems to work
very well.

The advantage of a tool like Travis is that your branches and pull requests
will be compiled against each version of GHC specified, which helps make sure
we're supporting older GHCs even if nobody on the team is running them.

### Hackage Matrix Builder
It is very useful to check the
[Hackage matrix builder](https://matrix.hackage.haskell.org) to check on older
versions of our own libraries, to see whether they need any bounds
adjustments. It is also useful to check the state of packages you intend to
depend on, as discussed above.

### Hackage Metadata Revisions
When versions of our package start going red in the hackage matrix builder,
there is a solution. Hackage accepts metadata revisions, meaning version bounds
can be altered retroactively. This feature is very useful to fix
red entries in the matrix, or fence off versions which are known to be bad,
for instance if they include a bug.

The existence of this feature is not a good reason to leave off upper bounds;
upper bounds are preventative, whereas metadata revisions are merely
curative.

