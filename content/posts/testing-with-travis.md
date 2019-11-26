---
title: Automatic Testing of Haskell Projects with Travis CI
date: 2019-11-24
project: infra
authors: gwilson
---

In this post you will learn how to quickly and easily set up continuous integration for your open source Haskell projects hosted on GitHub. Every time you push a commit, a free service called Travis CI will compile your code and run any test suites you have. I find this extremely valuable and I hope you will too. Best of all, there's very little effort involved.

Before we begin, this guide is for projects built with cabal. If you primarily use `stack` or `nix` for building your code, this post will not be very useful to you.

## Motivation

This kind of automated testing is great because it helps me catch errors on all the GHC versions my code claims to support. This is much easier than testing with every GHC version every time I want to push a commit, and gives me higher confidence. Furthermore, as a contributor to other people's open source projects, I feel more confident that my pull request is correct when Travis or other CI passes. As a maintainer, this also helps me screen incoming pull requests on my own projects.

## Instructions

For Travis to run, you need to enable Travis on your repository at [https://travis-ci.org/](https://travis-ci.org/). If you visit that link, you should be able to sign in with your GitHub account and enable the repository by following their instructions.

Next, you need a `.travis.yml` script to your repository to tell Travis how to build your project.

### Generating a `.travis.yml`

There is a tool to automatically generate .travis.yml for Haskell projects, called `haskell-ci`. You could always write your own `.travis.yml` file, but I prefer this way because `haskell-ci` generates scripts that are very featureful, including things like caching to speed up subsequent builds, and I get to offload knowing how Travis works.

You can install `haskell-ci` from Hackage:

```sh
$ cabal new-update
$ cabal new-install haskell-ci
```

You should now have `haskell-ci` available at your shell, assuming you have already added `~/.cabal/bin` to your `PATH`.

Next, you need to add some metadata to your cabal file: the `tested-with` field. This field will tell `haskell-ci` which GHC versions you want to test.

Add a line like the following to your cabal file:

```haskell
tested-with: GHC == 8.8.1
```

If you support more than one GHC version in your project, you can comma separate them:

```haskell
tested-with:
  GHC == 8.8.1,
  GHC == 8.6.5,
  GHC == 8.4.4
```

Now you can run `haskell-ci` on your cabal file to generate a `.travis.yml` file:

```sh
$ haskell-ci my-awesome-package.cabal
```

If you're using a multi-package project with a `cabal.project` file, you can instead run:

```sh
$ haskell-ci cabal.project
```

This gives you CI that builds all the packages in your project together. You can find an example of this in [the repository for sv](https://github.com/qfpl/sv), QFPL's CSV library. You will need to add `tested-with` metadata to each of the cabal files in your project, and they have to agree on GHC versions.

Now you can commit your `.travis.yml` file, push the commit to GitHub, and a Travis build should be automatically kicked off. Now you're good to go!

### Maintenance

When a new GHC version is released, update your library to support it and then update your `tested-with` clause. You will also need to update `haskell-ci` itself, since it needs a new release to learn about the new GHC version.

```sh
$ cabal new-update
$ rm ~/.cabal/bin/haskell-ci # delete the old binary
$ cabal new-install haskell-ci # install the new version
```

Now you can run `haskell-ci` again as above. That's all.

## Conclusion

I find Travis CI with the `haskell-ci` tool a very low effort way to test every commit of my Haskell code against multiple GHC versions. That is very valuable to me, and I hope it is valuable to you too.
