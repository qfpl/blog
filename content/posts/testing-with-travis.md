---
title: Automatic Testing of Haskell Projects with Travis CI
date: 2019-09-29
project: infra
authors: gwilson
---

In this post you will learn how to quickly and easily set up continuous integration for your open source Haskell projects hosted on GitHub. Every time you push a commit, a free service called Travis CI will compile your code and run any test suites you have. I find this extremely valuable and I hope you will too.

Before we begin, this guide is for projects built with cabal. If you primarily use `stack` or `nix` for building your code, you will not find much help here.

## Motivation

This kind of automated testing helps me quickly catch errors if I forget to locally test my code. More importantly, I like to support multiple GHC versions so that more users can use my code and automated tests can be run for every GHC version that my code claims to support. This is much easier than testing with every GHC version every time I want to push a commit, and gives me higher confidence.

Not only that, but as a contributor to other people's open source projects, I feel more confident that my pull request is correct when Travis or other CI passes.

## Instructions

For Travis to run, you need to enable Travis on your repository at [https://travis-ci.org/](https://travis-ci.org/). If you visit that link, you should be able to sign in with your GitHub account and enable the repository by following their instructions.

Now you need a `.travis.yml` script to your repository to tell Travis how to build your project.

### Generating a `.travis.yml`

I find automatically generating `.travis.yml` scripts much easier than manually writing them. Thankfully there is a tool to do just that for your Hsakell projects, called `haskell-ci`. You could always write your own `.travis.yml` file, but I prefer this way because `haskell-ci` generates scripts that are very featureful, including things like caching to speed up subsequent builds, and I get to offload knowing how Travis works.

You can install `haskell-ci` from Hackage:

```
$ cabal new-update
$ cabal new-install haskell-ci
```

You should now have `haskell-ci` available at your shell, assuming you have already added `~/.cabal/bin` to your `PATH`.

Next you need to add some metadata to your cabal file. It's the `tested-with` field. This field will tell `haskell-ci` which GHC versions you want to test.

Add a line like the following to your cabal file:

```
tested-with: GHC == 8.8.1
```

If you support more than one GHC version in your project, you can comma separate them:

```
tested-with:
  GHC == 8.8.1,
  GHC == 8.6.5,
  GHC == 8.4.4
```

Now you can run `haskell-ci` on your cabal file to generate a `.travis.yml` file:

```
haskell-ci my-awesome-package.cabal -o .travis.yml
```

If you're using a multi-package project with a `cabal.project` file, you can instead run:

```
$ haskell-ci cabal.project -o .travis.yml
```

and get a CI that builds all the packages in your project together. You can find an example of this in [the repository for sv](https://github.com/qfpl/sv), QFPL's CSV library.

You will need to add `tested-with` metadata to each of the cabal files in your project, and they have to agree on GHC versions.

### Maintenance

When a new GHC version is released, update your library to support it and then update your `tested-with` clause. You will also need to update `haskell-ci` itself, since it needs a new release to learn about the new GHC version.

```
$ rm ~/.cabal/bin/haskell-ci # delete the old binary
$ cabal new-install haskell-ci
```

Now you can run `haskell-ci` again as above. That's all.



## TODO (delete this section George)

TODO Stack section

TODO is the cabal travis script on hackage out of date?

TODO new project (not infra)
