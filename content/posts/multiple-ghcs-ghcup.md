---
title: Managing GHC versions with ghcup
date: 2019-09-18
project: infra
authors: gwilson
---

If you use Haskell, you probably use the Glasgow Haskell Compiler. GHC releases once every six months. When using `stack` or `nix`, the GHC version is managed for you. When using `cabal`, it is up to you to manage your GHC installation(s). I have [previously posted](../multiple-ghcs) about how I manage multiple installations of GHC using `nix` or `apt`. More recently I have mostly been using the `ghcup` tool, which lets you install and manage multiple GHC versions on most Linux distributions and macOS.

## Why `ghcup`?

I like `ghcup` because it's simple and works across multiple operating systems. For example, it's my favourite option for installing GHC on my home Fedora machine and my Ubuntu VMs (although Herbert's [Ubuntu PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc) and [Debian packages](https://downloads.haskell.org/debian/) should be preferred on those systems). On NixOS I still use nix.

`ghcup` is great for multiple getting GHCs happening, almost wherever you are, but you should consider using your operating system's package manager when possible.

## Using `ghcup`

Install `ghcup` by following [the instructions here](https://www.haskell.org/ghcup/). This will also install the recommended versions of `cabal` and GHC. Make sure to follow the script's instructions about adding the appropriate line to your `~/.bashrc` or similar.

With `ghcup` installed, you can install the extra GHC versions you care about:

```
$ ghcup install 8.0.2
$ ghcup install 8.2.2
$ ghcup install 8.4.4
$ ghcup install 8.6.5
```

$ You can also use `ghcup` to install the latest `cabal` as needed:

```
$ ghcup install-cabal
```

You will need to add `ghcup`'s binary directory `~/.ghcup/bin` to your `PATH` to use these executables.


`ghcup` will fetch binaries for your system, or if there aren't any available, it will try to build GHC from source. I've found the binaries work for me on all the Linux distributions I've tried. I've heard that `ghcup` works well on macOS too, but I haven't personally tested it. There are not many modern GHC binaries available for FreeBSD, so if you're using that operating system I recommend getting GHC from `pkg` instead.

## Specifying a GHC version to be used

You can select which GHC version is `ghc` on the `PATH` by using

```
$ ghcup set 8.8.1
```

which is very helpful.

If you want to use a different version for a particular build, you can use `cabal`'s `--with-compiler` flag or its short form `-w`. This flag lets you specify a Haskell compiler on your `PATH` to use, for example, you can test your project with GHC `8.4.4` by running

```
$ cabal v2-test -w ghc-8.4.4
```

I often use this when I'm testing my packages against multiple GHC versions.

That's it! Installing and managing multiple GHC versions has never been easier.

## Bonus information

### Setting a GHC per project

If you want a certain project to always use a particular GHC version, you can do so by adding a line to your `cabal.project` file. Here's a sample `cabal.project` file you could drop into any cabal project to make sure it always builds with GHC `8.4.4`:

```
packages: .
with-compiler: ghc-8.4.4
```

Now GHC `8.4.4` will be used regardless of which GHC is `ghc` on the `PATH`. This saves us having to use `-w` for every command.


### What about Windows?

`ghcup` does not work on Windows, and it is not planned to. So how should we manage our GHC versions on Windows? I have done some Haskell development on Windows. Most of the time I got away with WSL; I opened a bash shell backed by Ubuntu and just used `ghcup`.

On actual Windows, I have heard great things about Chocolatey. [See this blog post](https://hub.zhox.com/posts/introducing-haskell-dev/) for more information on getting GHC and cabal up and running on Windows.


### `curl` into `bash` is evil!

I agree. Here's how I actually use `ghcup`. Instead of curling into bash, clone the repo:

```
$ git clone git@github.com:haskell/ghcup.git
$ cd ghcup
```

Next, get yourself set up by running the script you would have fetched with curl:

```
$ ./bootstrap-haskell
```

The rest of the process is as above.
