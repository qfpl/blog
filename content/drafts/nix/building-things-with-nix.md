---
title: Building things with Nix
date: 2017-07-27
authors: dlaing
project: infra
---

## An overview of building things with Nix

Let's create a package for the GNU hello utility.
We're going to travel quickly and gloss over some things, but we'll dig into the details immediately afterwards.
For now I just want you to set up an example package and give you a sense of what happens when Nix builds something successfully.

To that end, we'll write `hello.nix`:
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
to package up the GNU hello utility.

This is a function that takes two arguments and produces a derivation - and the derivation is what we are after.

There is a function in the Nix package set named `callPackage` that is helpful here.
The `callPackage` function will fill in missing arguments if the argument names match the names of packages from the Nix package set.

We can write `default.nix` to demonstrate this:
```
{ nixpkgs ? import <nixpgkgs> {} }:

nixpkgs.callPackage ./hello.nix {}
```

The missing arguments to `hello.nix` have been filled in by `callPackage`, and so `default.nix` gives us a derivation.

The GNU hello utility is built with `autotools` - `configure`, `make` and friends - and Nix will try to use those if you don't give it any other information about how to build a derivation.

This means we can build the derivation with:
```
nix-build default.nix
...
/nix/store/g0fw64zf7n0hr1dx7yl9n8qgqbhdngrm-hello-2.10
```
and 

If no file is given to `nix-build` it will look for `default.nix` in the current directory, so we can do:
```
nix-build
```
and get the same result.

The output will appear in the symbolic link named `result`:
```
> ls result
bin 
share
```

This symbolic link is set up as a garbage collection root, so your package and its dependencies will stick around until you remove `result` and a garbage collection occurs.

We could run the executable:
```
> result/bin/hello
Hello, world!
```
but we'd feel a bit bad for whoever had to muck about with autotools to package something so simple, so maybe we shouldn't.

We're about to look at all of this in greater detail, but before we do that we should clean up after ourselves:
```
> rm result
> nix-collect-garbage -d
```

## What is going on under the hood

### Getting hold of our sources

In order to make use of the tarball containing the GNU hello source code, we need to know the hash of the sources.

Thankfully there are some helpful scripts for this, which we'll install:
```
nix-env -i nix-prefetch-scripts
```

We can use these scripts to download things, add them to the nix store, and to print their hashes and other metadata that we might need.

Let's grab a hold of those sources now:
```
> nix-prefetch-url mirror://gnu/hello/hello-2.10.tar.gz
downloading ‘http://ftpmirror.gnu.org/hello/hello-2.10.tar.gz’... [0/0 KiB, 0.0 KiB/s]
path is ‘/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz’
0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i
```

We can see that there is a copy in the Nix store, and the script prints out the hash of the tarball as well.

At the moment the prefetch scripts contain:

- nix-prefetch-bzr
- nix-prefetch-cvs
- nix-prefetch-git
- nix-prefetch-hg
- nix-prefetch-svn
- nix-prefetch-url

These have corresponding functions which we use inside our Nix packages:
```
src = fetchurl {
  url = "mirror://gnu/hello/${name}.tar.gz";
  sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
};
```
so that other people can use them without having to do any prefetching.

Once you know the hash of the sources, you are good to go.
That is why `default.nix` was able to download and build the sources at the start of this post, before we did the prefetch.

There is one small optimization available for GitHub users which is worth pointing out.
Fetching from git will checkout the whole repository in order to get hold of a specific revision:
```
exitcode = pkgs.fetchgit {
  url = "https://github.com/qfpl/exitcode";
  rev = "e56313946fdfe77eed91c54d791b7be8aa73c495";
  sha256 = "0a2nnk9ifaln0v9dq0wgdazqxika0j32vv8p6s4qsrpfs2kxaqar";
};
```
but f the project we are interested in is hosted on GitHub, there is a function that can use the GitHub APIs to fetch the code for a specific revision:
```
exitcode = pkgs.fetchFromGitHub {
  owner = "qfpl";
  repo = "exitcode";
  rev = "e56313946fdfe77eed91c54d791b7be8aa73c495";
  sha256 = "0a2nnk9ifaln0v9dq0wgdazqxika0j32vv8p6s4qsrpfs2kxaqar";
};
```
which can save us some time and data.

### Building the project from `nix-shell`

We have `nix-build` available to us for building Nix derivations.

While we're working on writing our own derivations, we can use `nix-shell`
```
> nix-shell default.nix
nix-shell >
```

If no file is given to `nix-shell` it will look for `shell.nix` in the current directory, and then `default.nix`, so we can do:
```
> nix-shell
nix-shell >
```

By default we'll have access to everything from our host environment while we're inside of the shell, so that we can edit files and so on.
If we want to make sure that we have specified all of the required dependencies to build our package, we should use:
```
> nix-shell --pure
```
so that the only things we have access to are the things mentioned in our Nix files.

We would normally use separate `shell.nix` and `default.nix` files if we wanted to tweak the packages and environment that is available while we're developing a package.
This might include things like adding a debugger or an editor as a dependency in `shell.nix`, or setting an environment variable that leads to more verbose output as things build.

Let's have a look at what we have at our disposal once we're inside the shell.
Along with a few standard build tools, we have access to the attributes from `hello.nix` as environment variables.

The `name` attribute is present:
```
nix-shell> echo $name
hello-2.10
```

The `src` attribute is present, and is a path to the sources in the Nix store:
```
nix-shell> echo $src
/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz
nix-shell> ls $src
/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz
```

The hash of this derivation has already been calculated at this point, so Nix has added an environment variable pointing to the directory where we should put our results:
```
nix-shell> echo $out
/nix/store/g0fw64zf7n0hr1dx7yl9n8qgqbhdngrm-hello-2.10
```
but it hasn't been created yet:
```
nix-shell> ls $out
ls: cannot access '/nix/store/g0fw64zf7n0hr1dx7yl9n8qgqbhdngrm-hello-2.10': No such file or directory
```

Give all of that information, we can use `autotools` to build the GNU hello utility:
```
nix-shell> tar zxf $src
nix-shell> cd hello-*
nix-shell> ./configure --prefix=$out
nix-shell> make
nix-shell> make install
```
and then run it directly from the Nix store:
```
nix-shell> exit
> /nix/store/g0fw64zf7n0hr1dx7yl9n8qgqbhdngrm-hello-2.10/bin/hello
Hello, world!
```

Let us clean this up for our next little adventure:
```
> nix-collect-garbage -d
```

### Building the project with `builder.sh`

We can collect these build steps into a bash script, which we'll name `builder.sh`:
```
source $stdenv/setup

tar zxvf $src
cd hello-*
./configure --prefix=$out
make
make install

echo "The script actually ran"
```

The `$stdenv/setup` step on the first line is setting up the Nix environment for us.
The echo on the last line is so that we can distinguish this build from the automatic build steps that Nix did for us during the overview.

If we reference this script from the `builder` attribute in our Nix package:
```
{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "hello-2.10";
  
  builder = ./builder.sh;

  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
  };
}
```
then Nix will use it build the package:
```
> nix-build
...
The script actually ran
/nix/store/p945zy2qhfbk6rb5gvyqclymyvwx5z7q-hello-2.10
```

This gives us a different hash to what we had before, which is expected.
If we changed the builder script:
```
source $stdenv/setup

echo "A different builder"

tar zxvf $src
cd hello-*
./configure --prefix=$out
make
make install
```
we can see that the builder script takes part in the hash computation as well:
```
nix-shell> echo $out
/nix/store/7hd6g7hpdg21h5nrzijicim6lik9qsjr-hello-2.10
```

We can also inline the builder script by making use of some of the utility functions that Nix provides along with the multi-line string literals:
```
{ pkgs, stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "hello-2.10";
  
  builder = pkgs.writeText "builder.sh" ''
    source $stdenv/setup

    tar zxvf $src
    cd hello-*
    ./configure --prefix=$out
    make
    make install

    echo "The script actually ran"
  '';

  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
  };
}
```

### Debugging a nix build

We're now going to sabotage our builder, to see what we can do to correct things.

We'll deliberately change into the wrong directory:
```
source $stdenv/setup

tar zxvf $src
cd hello
./configure --prefix=$out
make
make install

echo "The script actually ran"
```
resulting in
```
> nix-build
...
/nix/store/s143n1fws6lb0ngnk6bm6ggrdxkxg8c8-builder.sh: line 4: cd: hello: No such file or directory
builder for ‘/nix/store/p9lvjiik8pwv4rlpxsl29as151vrwp2z-hello-2.10.drv’ failed with exit code 1
error: build of ‘/nix/store/p9lvjiik8pwv4rlpxsl29as151vrwp2z-hello-2.10.drv’ failed
```

What can we do?

We can indicate that we want to keep the temporary build directory in the event of failures:
```
> nix-build -K
/nix/store/s143n1fws6lb0ngnk6bm6ggrdxkxg8c8-builder.sh: line 4: cd: hello: No such file or directory
note: keeping build directory ‘/tmp/nix-build-hello-2.10.drv-0’
builder for ‘/nix/store/p9lvjiik8pwv4rlpxsl29as151vrwp2z-hello-2.10.drv’ failed with exit code 1
error: build of ‘/nix/store/p9lvjiik8pwv4rlpxsl29as151vrwp2z-hello-2.10.drv’ failed
```

We can have a look in this directory:
```
> cd /tmp/nix-build-hello-2.10.drv-0
> ls
env-vars hello-2.10
```
and we'll see that we have the unpacked sources for the GNU hello utility, along with a file naamed `env-vars`.

This file contains the environment variables at the point of the failure, so we can source that file:
```
> source env-vars
```
and then try to carry out our build steps like we were in a `nix-shell` to see what went wrong:
```
> cd hello-2.10
> configure --prefix=$out
....
```

### Building the project with the generic builder

Instead of specifying a build script, we can let Nix's generic builder do some of the work for us.

The generic builder proceeds through a number of phases, which we can overload if we want:
```
{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "hello-2.10";

  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
  };

  unpackPhase = ''
    tar zxvf $src
    cd hello-*
  '';

  configurePhase = ''
    ./configure --prefix=$out
  '';

  buildPhase = ''
    make
  '';

  installPhase = ''
    make install
  '';
}
```

There is a default set of phases that get run, and there are default activities that get run in each phase.
We can edit the list of phases to run, either to add new phases, to reorder them, or to skip some of them:
```
  phases = [installPhase];
```

Each of these phases has a default implementation that is usually pretty sensible.
We can see that by removing our `unpackPhase` and letting the generic build functionality take over:
```
{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "hello-2.10";

  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
  };

  configurePhase = ''
    ./configure --prefix=$out
  ''';

  buildPhase = ''
    make
  '';

  installPhase = ''
    make install
  '';
}
```

We can chip away at that, verifying that the output is the same every time we remove one of our phases.
Eventually we'll end up where we started at the beginning of this post:
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
and if you look through the Nixpkgs package set you'll see that there are a lot of packages that aren't much more complicated than this.

If you have a look at the [Nixpkgs manual](https://nixos.org/nixpkgs/manual), you'll see that there all kinds of settings that allow you tweak the various phases.
So instead of adding your own `configurePhase` to slightly amend what the generic build functionality was doing for you, you can do:
```
{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "hello-2.10";

  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
  };
  
  configureFlags = '--enable-extra-awesomeness';
}
```

## Dealing with dependencies

TODO section on the different kinds of dependencies

## Where to go for more information

The Nix manual has a [chapter](https://nixos.org/nix/manual/#ch-simple-expression) covering the basics of writing packages, and the Nixpkgs manaual has a [chapter](https://nixos.org/nixpkgs/manual/#chap-stdenv) that goes into much more detail.
The Nixpkgs manual also has chapters on the Nix support for various languages and frameworks, which can be very handy.

You can also browse around in [the `nixpkgs` pacakge set](https://github.com/NixOS/nixpkgs) and have a look at how other packages are built.

<!--
## Pinning `nixpkgs`

TODO subsection on using this to pin nixpkgs
TODO   comment on all of the places where you can reach out to get, or a tarball, or whatever

## Built-in functions and helpers

TODO commands, writeScriptBin, that sort of thing
-->
