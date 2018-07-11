---
title: Installing and Running Multiple GHC Versions
date: 2018-07-09
project: infra
authors: gwilson
---

In my roles both as a
library author and as a Hackage Trustee, I find myself needing to run builds
against multiple versions of the Glasgow Haskell Compiler on a daily basis.
I thought it would be worth putting together a post on how I manage my
installations of GHC and associated tools like cabal-install.

### Ubuntu

The easiest distribution on which to install multiple GHC versions is Ubuntu.
I have an Ubuntu virtual machine on a server, onto which I offload some slow
builds so as not to tie up my local work machine. Herbert Valerio Riedel has
set up an [Ubuntu PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc)
from which one can install multiple GHC, cabal, happy, and alex versions.
These will happily sit side-by-side, and the PPA page offers documentation
on switching what the `ghc` symlink points to.

After adding the PPA to my repositories by following the instructions, I
installed the software I needed with a command something like the following:

```
sudo apt install ghc-8.4.3 ghc-8.2.2 ghc-8.0.2 ghc-7.10.3 ghc-7.8.4 ghc-7.6.3 ghc-7.4.2 ghc-7.2.2 ghc-7.0.4 alex-3.1.7 cabal-install-2.2 happy-1.19.5
```

**Update (2018-07-11)** Herbert has informed me that he also has a [repository for Debian](https://downloads.haskell.org/debian/).

### NixOS

On our work stations, my team and I run [NixOS](https://nixos.org/), a Linux
distribution managed using the functional programming language Nix.
In my `configuration.nix` file, I have two pinned snapshots of nixpkgs -
one for providing rather old GHC versions, and one for providing bleeding-edge
GHC versions.

The following is an excerpt of my `configuration.nix` file:
```nix
{ config, pkgs, ... }:

let
  oldghcs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/83b35508c6491103cd16a796758e07417a28698b.tar.gz) {
    config = config // { allowBroken = true; };
  };
  newghcs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/f682ff93a2778f101d93b68c97278f902523758a.tar.gz) {
    config = config // { allowBroken = true; };
  };
in {
  environment.systemPackages = with pkgs; [
    oldghcs.haskell.compiler.ghc704
    oldghcs.haskell.compiler.ghc722
    oldghcs.haskell.compiler.ghc742
    oldghcs.haskell.compiler.ghc763
    oldghcs.haskell.compiler.ghc784
    haskell.compiler.ghc7103
    haskell.compiler.ghc802
    haskell.compiler.ghc822
    newghcs.haskell.compiler.ghc843
    newghcs.haskell.compiler.ghc861

    # other entries...
  ];

  # other configuration...
}
```

This setup gives me every major compiler version back to GHC 7.0.
`newghcs` was simply the most recent commit in nixpkgs when I set it up, and
that is where I get GHC 8.4.3 and GHC 8.6 alpha, since they are not in my main
nixpkgs, which is nixos-18.03.
`oldghcs` is a commit carefully chosen from history, as the following commit
dropped GHC 7.2.2 from nixpkgs, and I wanted to have the latest release in the
GHC 7.2 series installed.
nixos-18.03 gives me `7.10.3`, `8.0.2`, and `8.2.2`

### Using these GHC versions

Using either of the above methods, we will find our GHC versions on the `PATH`.
To run GHC version 7.10.3, we run `ghc-7.10.3`, and the same pattern works for
other versions.

When I want to build one of my projects using a particular GHC version, I
configure cabal using the the
[`-w` flag](https://cabal.readthedocs.io/en/latest/installing-packages.html?highlight=--with-compiler#cmdoption-setup-configure-with-compiler)
(or its long form `--with-compiler`).
I use cabal's nix-style local builds for all my Haskelling. This is also known
as the cabal new-build workflow, and it was added in cabal 1.24.
My invocations look like this:

```
cabal new-configure -w ghc-8.2.2
```

This writes a `cabal.project.local` file, so subsequent calls to `cabal new-build`,
`cabal new-test` and so on will use the configured GHC version. To change to
another GHC version, we can run `cabal new-configure` again with a different
GHC version specified.
This way, I can install all my GHCs side-by-side, and choose whichever I need
to for what I want to build.

A GHC version can also be fixed on a per-project basis using a `cabal.project`
file. For example, if we want to use `ghc-8.0.2` for our project, we can include
this `cabal.project` file:

```nix
packages: ./
with-compiler: ghc-8.0.2
```

and cabal will use `ghc-8.0.2` without us needing to specify via `-w`.

### Conclusion

That's how I install and run multiple different GHC versions on my systems.

Happy Haskell Hacking!
