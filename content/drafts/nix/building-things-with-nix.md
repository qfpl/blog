---
title: Building things with Nix
date: 2017-07-27
authors: dlaing
project: infra
---

# Building GNU `hello`

Let's create `hello.nix`:
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

## Using `nix-build`

## Using `nix-shell`

# Getting comfortable with building things with Nix

TODO make sure we understand builders, phases, and dependencies

TODO make sure we know how to get hold of things from Git etc...
