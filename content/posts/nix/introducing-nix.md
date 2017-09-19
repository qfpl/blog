---
title: Introducing Nix
date: 2017-07-26
authors: dlaing
project: infra
---

# What is Nix?

Nix is a purely functional package manager.

Every package has a hash based on everything that was used to build it, and the package outputs are placed in an immutable store that is keyed on that hash.

The [About Nix](http://nixos.org/nix/manual/#ch-about-nix) chapter of the Nix manual provides a more detailed overview of the benefits of Nix.
In summary, we get these benefits:

- we have a simple functional language that can be used to describe these packages
- our package descriptions always have a complete description of their dependencies
- we can share a cache of build artifacts and know that we're getting the same outputs as if we'd built everything from the source code ourselves
- we have atomic upgrades and rollbacks
- we can have multiple versions of packages installed
- multiple non-privileged users installing packages without issue

That idea can be extended to managing a whole Linux distribution (NixOS) a fleet of machines (NixOps), but we're going to start by looking at the package manager.

# What is a Nix derivation?

A Nix derivation describes how to build something.
It could be an executable, a library, a website, or anything really.

A Nix package is usually a function which takes any build-time or run-time dependencies as arguments and produces a derivation.
Sometimes there are configuration options as well.

The dependencies which are supplied as arguments are _also_ derivations.

For every derivation you can compute a deterministic hash - the same inputs will lead to the same outputs.

The inputs to the hash of a derivation come from the description of the derivation and the hashes of all of dependencies.

So the hash will change if:

- you change the compiler flags your program uses
- you change the sources you rely on
- you change the version of your compiler
- you change the compiler flags of one of your dependencies

This is what gives Nix high levels of reproducibility. 
If we each have a derivation with the same hash, it is almost always going to build the same thing for each of us.

There can be some side effects of some build processes that might make things non-deterministic, like build steps that rely on file modification dates.
There are Nix tools which can take care of thse 

The hashing scheme also enables binary substitution.
If I store the outputs of the derivations that I have built, then I can share these as a Nix cache, keyed on the hashes of the deriviations.

## An example derivation

The Nix package for the GNU `hello` program is pretty simple.

It takes two derivations as inputs.

- `stdenv` provides common functionality used to make derivations, along with archive handling tools, a C/C++ compiler, and the usual autotools.
- `fetchurl` is used to get hold of the source files

```
{ stdenv, fetchurl }:
```

It uses `stdenv` to start building a derivation...

```
stdenv.mkDerivation rec {
```

with a given name...

```
  name = "hello-2.10";
```

and a given set of sources (along with their hash)...

```
  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
  };
```

and that's all it needs.

```
}
```

Nix has some default behaviors which are simplifying things for us here.

- if it can deal with an archive format, it will unpack any sources for you
- if no other build instructions are provided, it will run `configure; make; make install` 

In the case of GNU `hello`, that is all we need.

Put together, it looks like this:
```
{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "hello-2.10";

  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
  };
}
```

# What is a Nix package collection?

A Nix package collection is a set of Nix packages - functions which take derivations as inputs and produce derivations as outputs - along with the Nix code required to wire things up so that all of the derivations are given their dependencies from the other packages within the package collection.

This means that the contents of the package tree are ready-to-build derivations, since all of these deriviation-producing functions have been provided with their required arguments.

You can grab the common package set used by the community here:
```
git clone https://github.com/NixOS/nixpkgs
```
which has different branches that people use.

You can fork the community package set, write your own package set, write extensions to the community package set - you can slice and dice these more or less any way that you want.
The easiest way to manage these is via channels, which I'll cover once you get started with Nix.

# Next up

For now, those are the definitions that you need to know about.

If you're ready to get started, we begin diving in [here](../getting-started-with-nix/).
