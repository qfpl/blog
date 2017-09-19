---
title: The QFPL infrastructure work
---

This is the main page for all things related to how we at QFPL are setting up our infrastructure.

This includes this blog, and the Nix / Hydra / Hail tooling that builds it.

## Hydra & Hail

QFPL projects are built using [Hydra for continuous integration](http://hydra.qfpl.io/) and deployed using [Hail](https://github.com/TaktInc/hail).

## Hakyll

The blog is written using [Hakyll](https://jaspervdj.be/hakyll/) - a static site generator written in Haskell - and is managed with [Nix](http://nixos.org/nix/).

If you're a team member, you probably want to read about [writing for the FP blog](../../posts/writing-for-the-fp-blog/).

You may also be interested in browsing around the [git repository](https://github.com/qfpl/blog) to see what we've done with Hakyll.

## Nix

Nix is a package manager based on the ideas from pure functional programming.

There is some general information about Nix that should be useful to those who want to get started with it:

- [Introducing Nix](../../posts/nix/introducing-nix/)
- [Getting started with Nix](../../posts/nix/getting-started-with-nix/)
- [Building things with Nix](../../posts/nix/building-things-with-nix/)

<!--
If you're doing Haskell development, Nix is great for that:

- [Working with Haskell and Nix](../../posts/nix/working-with-haskell-and-nix)
- [Mixing Haskell and non-Haskell dependencies with Nix](../../posts/nix/mixing-haskell-and-non-haskell-dependencies-with-nix)

Whether you are using Nix with Haskell or not, you can also make use of Hydra and Hail to build and deploy your code:

- [Using Hydra for Continuous Integration](../../posts/nix/using-hydra-for-ci)
- [Using Hail for Continuous Deployment](../../posts/nix/using-hail-for-cd)
-->

