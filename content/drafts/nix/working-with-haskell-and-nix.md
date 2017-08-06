---
title: Working with Haskell and Nix
date: 2017-07-28
authors: dlaing
project: infra
---

## Working with a basic package

We're going to work through what was involved in Nix-ifying the [`exitcode`](https://github.com/qfpl/exitcode) package.

It is a good example of the usual process for getting a cabal-based project working with Nix.

If you want to work along with this example, you'll need to delete the various `.nix` files from your checkout of `exitcode`.

### Using `cabal2nix`

We can take care of a lot of the boring work of Nix-ifying projects by using the `cabal2nix` program.

Let's install it now:
```
nix-env -i cabal2nix
```
and then run it from the root directory of the `exitcode` project:
```
> cd exitcode
> cabal2nix . > exitcode.nix
```

We end up with this:
```
{ mkDerivation, base, checkers, lens, mtl, QuickCheck
, semigroupoids, stdenv, tasty, tasty-hunit, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "exitcode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base lens mtl semigroupoids transformers
  ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/exitcode";
  description = "Monad transformer for exit codes";
  license = stdenv.lib.licenses.bsd3;
}
```
which is a function that will produce a derivation once it is given the appropriate arguments.

In this case, the arguments are mostly Haskell libraries.

### Using `nix-build`

TODO create default.nix

```
{ nixpkgs ? import <nixpkgs> {}}:
let
  inherit (nixpkgs) pkgs;
in
  pkgs.haskellPackages.callPackage ./exitcode.nix {};
```

TODO use nix-build, show what happens

```
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};
in
  haskellPackages.callPackage ./exitcode.nix {};
```


### Using `nix-shell`

TODO create shell.nix

```
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;
  drv = import ./default.nix { inherit nixpkgs compiler; };
in
  if pkgs.lib.inNixShell then drv.env else drv
```

TODO use nix-shell, show what happens

### Using different versions of GHC

## Depending on packages from elsewhere

The [`bomradar`](https://github.com/qfpl/bomradar) package was developed at around the same time as, and depends on, `exitcode`.

Let us have a look at the various ways of bringing in dependencies from outside of `nixpkgs` into our project.

### Using packages from your filesystem


```
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      exitcode = import ../exitcode { inherit nixpkgs compiler; };
    };
  };

in
  modifiedHaskellPackages.callPackage ./bomradar.nix {};
```

This is fine for developing a group of inter-related packages locally.
The drawback is that it's going to be a pain for other people to build the pacakge, unless they have copies of all of your project sources and have them placed in the correct relative locations.

### Using packages from git

We can improve matters once our packages are in source control.

In order to make use of a copy of `exitcode` from elsewhere, we need to know the hash of the sources.

Thankfully there are some helpful scripts for this, which we'll install:
```
nix-env -i nix-prefetch-scripts
```

We can use these scripts to download things, add them to the nix store, and to print their hashes and other metadata that we might need.

Let's use it to grab a copy of `exitcode` from the git repository:
```
> nix-prefetch-git https://github.com/qfpl/exitcode
...
{
  "url": "https://github.com/qfpl/exitcode",
  "rev": "e56313946fdfe77eed91c54d791b7be8aa73c495",
  "date": "2017-08-03T15:16:33+10:00",
  "sha256": "0a2nnk9ifaln0v9dq0wgdazqxika0j32vv8p6s4qsrpfs2kxaqar",
  "fetchSubmodules": true
}
```
or, if we wanted to specify a particular branch, we can do:
```
> nix-prefetch-git https://github.com/qfpl/exitcode refs/heads/master
```

TODO use with fetchgit, for a nixified project
```
  sources = {
    exitcode = pkgs.fetchgit {
      url = "https://github.com/qfpl/exitcode";
      rev = "e56313946fdfe77eed91c54d791b7be8aa73c495";
      sha256 = "0a2nnk9ifaln0v9dq0wgdazqxika0j32vv8p6s4qsrpfs2kxaqar";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      exitcode = import sources.exitcode { inherit nixpkgs compiler; };
    };
  };
```

TODO for a non-nixified project
```
  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      exitcode = super.callCabal2nix "exitcode" sources.exitcode {};
    };
  };
```

Using `fetchgit` requires a checkout of the whole repository, which might consume more data and/or time than we would like.

If the repository is hosted on GitHub, we can use `fetchFromGitHub` to grab a tarball which only contains the code for a particular revision:
```
  sources = {
    exitcode = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "exitcode";
      rev = "e56313946fdfe77eed91c54d791b7be8aa73c495";
      sha256 = "0a2nnk9ifaln0v9dq0wgdazqxika0j32vv8p6s4qsrpfs2kxaqar";
    };
  };
```

It is worth pointing out that there is nothing special about `sources` object that we've been using.
Gathering sources like that is just a stylistic choice of mine.

If you'd prefer to inline the fetching into the package override, go for it:
```
  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      exitcode = import (pkgs.fetchFromGitHub {
          url = "https://github.com/qfpl/exitcode";
          rev = "e56313946fdfe77eed91c54d791b7be8aa73c495";
          sha256 = "0a2nnk9ifaln0v9dq0wgdazqxika0j32vv8p6s4qsrpfs2kxaqar";
        }) { inherit nixpkgs compiler; };
    };
  };
```

### Using package versions from Hackage

```
  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      exitcode = super.callHackage "exitcode" "0.1" {};
    };
  };
```

## Setting up a local Hoogle instance

## Tweaking common things in packages

TODO a tour of pkgs.haskell.lib

## Tweaking less common things in packages

TODO overridecabal, a tour of genericbuilder.nix
