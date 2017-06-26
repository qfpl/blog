---
title: Getting started with Hydra
date: 2017-05-23
project: infra
authors: dlaing
---

## What is Hydra?

Hydra is a continuous integration service based on the Nix packaging system.

I should probably back up a little and talk about what Nix is and why you might want to use it.

Nix expressions describe how to build software, and each expression is stored with the hash of what went into building the software.
This includes the source code along with the hashes of all of the software required to build the code - so thinks like your C compiler, your python interpreter or the version of build tools that you use.

If you change anything - from your source code, to your compiler flags, to the version of the compiler you are using - it will change the hash of your Nix expression.
This means that when someone else builds your Nix expression, they are building it with exactly the same tools as you.

A consequence of this high level of reproduciblity is that if you have the binary outputs of the build of a nix expression for a particular OS, you can store them in a binary cache.
If your local machine is configured to use a binary cache, it will search the cache for binary outputs matching the hash of the nix expression.
If it finds something, it can install it directly without having to build anything, and you can be confident that you have installed the same thing that you would have got from building everything locally yourself.

Hydra can be configured to watch various repositories and to build certain nix expressions contained in them on a fleet of build machines.
It will also setup Nix channels - so that people can install build artifacts and keep them up to date - and if you prepare some signing keys it can also establish a binary cache that your team can use.

Once it is setup, it's pretty easy to work with, so let us get started.

## Installation

If you expand on the idea of Nix, you can describe an entire OS and its environment with a Nix expression.

This is what NixOS does, and we'll be using that to set up a Hydra server.

The file that configures the OS is `/etc/nixos/configuration.nix`.

A simple version of such a file that sets up PostgreSQL and installs git and vim would be:
```nix
{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  time.timeZone = "Australia/Brisbane";

  environment.systemPackages = with pkgs; [
    gitAndTools.gitFull
    vim
  ];

  services.postgresql = {
      enable = true;
      package = pkgs.postgresql; 
  };
}
```

Once you have such a system setup - and if you enabled SSH to get inside of it - you can alter the file and then run `nixos rebuild switch` in order to update the system to match the file.

Since Nix uses an append only datastore, you can always rollback to a previous version if you run into trouble.

### Setting up the service in NixOS


```nix
  # Turn on postfix for sending emails
  services.postfix = {
      enable = true;
      setSendmail = true;
  };

  # Turn on postgresql for storing data
  services.postgresql = {
      enable = true;
      package = pkgs.postgresql; 
  };

  services.hydra = {
      # Turn on Hydra
      enable = true;  
      # Set the base URL for the Hydra instance
      hydraURL = "http://localhost";
      # Set the email address to send notifications to
      notificationSender = "hydra@localhost";
      # Set the file describing the build machines to use
      # - this is actually the default value, 
          so we don't need to add it here explicitly
      # - we will set this up later on
      buildMachinesFiles = [/etc/nix/machines];
  };

  # This allows these uses to install things using the various Nix systems
  nix.extraOptions = ''
    trusted-users = hydra hydra-evaluator hydra-queue-runner
  '';
```

If we want to see debug output from the server we can add:
```nix
   services.hydra = {
     ...
     debugServer = true;
     ...
   };
```
and then - since NixOS is using systemd - we can watch the log output from the Hydra services with:
```
sudo journalctl -f -u hydra-*
```

### Setting up a binary cache

We have some setting up to do if we want Hydra to populate a binary cache.

We need to generate a key pair, put it somewhere we can tell Hydra about, and make sure that it has the appropriate permissions:
```
> install -d -m 551 /etc/nix/hydra.localhost-1
> cd /etc/nix/hydra.localhost-1
> nix-store --generate-binary-cache-key hydra.localhost-1 ./hydra.localhost.sec ./hydra.localhost.pub
> chown -R hydra:hydra /etc/nix/hydra.localhost-1
> chmod 440 /etc/nix/hydra.localhost-1/hydra.localhost.sec
> chmod 444 /etc/nix/hydra.localhost-1/hydra.localhost.pub
```

We also need to set up a directory for Hydra to use as the cache:
```
install -d -m 755 /var/lib/hydra/cache
chown -R hydra-queue-runner:hydra /var/lib/hydra/cache
```

We then tell Hydra about it with these additions to the configuration:
```nix
  services.hydra = {
      ...
      extraConfig = ''
        # Point Hydra at the cache location and the secret key file:
        store_uri = file:///var/lib/hydra/cache?secret-key=/etc/nix/hydra.localhost/hydra.localhost.sec
        # The following lines are deprecated and will cause warnings to appear in the logs.
        # It's not clear yet if all of the usages of these options have been removed from Hydra yet,
        # so we're leaving them in there for now.
        # See https://github.com/NixOS/hydra/issues/435 for more information.
        binary_cache_secret_key_file = /etc/nix/hydra.localhost/hydra.localhost.sec
        binary_cache_dir = /var/lib/hydra/cache
      ''; 
      ...
  };
```

At this point we could do `nixos rebuild switch` to setup Hydra and all should be well.

It is also possible to automate the setup of the binary cache, using something like:
```nix
  systemd.services.hydra-cache-setup = {
    description = "Create binary cache for Hydra";
    serviceConfig.Type = "oneshot";
    serviceConfig.RemainAfterExit = true;
    wantedBy = [ "multi-user.target" ];
    requires = [ "hydra-init.service" ];
    after = [ "hydra-init.service" ];
    environment = config.systemd.services.hydra-init.environment;
    script = ''
      if [ ! -e ~hydra/.setup-is-complete ]; then
        # create signing keys
        /run/current-system/sw/bin/install -d -m 551 /etc/nix/hydra.example.org-1
        /run/current-system/sw/bin/nix-store --generate-binary-cache-key hydra.example.org-1 /etc/nix/hydra.example.org-1/secret /etc/nix/hydra.example.org-1/public
        /run/current-system/sw/bin/chown -R hydra:hydra /etc/nix/hydra.example.org-1
        /run/current-system/sw/bin/chmod 440 /etc/nix/hydra.example.org-1/secret
        /run/current-system/sw/bin/chmod 444 /etc/nix/hydra.example.org-1/public
        # create cache
        /run/current-system/sw/bin/install -d -m 755 /var/lib/hydra/cache
        /run/current-system/sw/bin/chown -R hydra-queue-runner:hydra /var/lib/hydra/cache
        # done
        touch ~hydra/.setup-is-complete
      fi
    '';
  };
```
TODO highlight the appropriate lines
which was inspired by [this piece of Nix code](https://github.com/peti/hydra-tutorial/blob/master/hydra-master.nix).

### Setting up build machines

To get started, we're going to set up the machine running Hydra as the single build machine.

In `/etc/nixos/configuration.nix` we had
```nix
  services.hydra = {
      ...
      buildMachinesFiles = [/etc/nix/machines];
      ...
  };
```
so let us set up that file:
```nix
# Hydra build machines
# SSHNAME system-types ssh-key max-jobs speed-factor suported-features mandatory-feature
localhost i686-linux,x86_64-linux - 6 1
```

We could also set that up in `/etc/nixos/configuration.nix`, using:
```nix
  ...
  nix.buildMachines = [
    {
      hostName = "localhost";
      systems = [ "i686-linux" "x86_64-linux" ];
      maxJobs = 6;
    }
  ];
  ...
```

TODO setting up remote build machines

### Setting up a emails 

TODO This will get sorted out when we have access to the necessary pieces

## The whole shebang

If we wanted to fold all of this into the one block of code, we would have:
```nix
  services.postfix = {
      enable = true;
      setSendmail = true;
  };

  services.postgresql = {
      enable = true;
      package = pkgs.postgresql; 
  };

  services.hydra = {
      enable = true;  
      hydraURL = "http://localhost";
      notificationSender = "hydra@localhost";
      extraConfig = ''
        store_uri = file:///var/lib/hydra/cache?secret-key=/etc/nix/hydra.localhost/hydra.localhost.sec
        binary_cache_secret_key_file = /etc/nix/hydra.localhost/hydra.localhost.sec
        binary_cache_dir = /var/lib/hydra/cache
      ''; 
  };

  systemd.services.hydra-cache-setup = {
    description = "Create binary cache for Hydra";
    serviceConfig.Type = "oneshot";
    serviceConfig.RemainAfterExit = true;
    wantedBy = [ "multi-user.target" ];
    requires = [ "hydra-init.service" ];
    after = [ "hydra-init.service" ];
    environment = config.systemd.services.hydra-init.environment;
    script = ''
      if [ ! -e ~hydra/.setup-is-complete ]; then
        # create signing keys
        /run/current-system/sw/bin/install -d -m 551 /etc/nix/hydra.example.org-1
        /run/current-system/sw/bin/nix-store --generate-binary-cache-key hydra.example.org-1 /etc/nix/hydra.example.org-1/secret /etc/nix/hydra.example.org-1/public
        /run/current-system/sw/bin/chown -R hydra:hydra /etc/nix/hydra.example.org-1
        /run/current-system/sw/bin/chmod 440 /etc/nix/hydra.example.org-1/secret
        /run/current-system/sw/bin/chmod 444 /etc/nix/hydra.example.org-1/public
        # create cache
        /run/current-system/sw/bin/install -d -m 755 /var/lib/hydra/cache
        /run/current-system/sw/bin/chown -R hydra-queue-runner:hydra /var/lib/hydra/cache
        # done
        touch ~hydra/.setup-is-complete
      fi
    '';
  };

  nix.extraOptions = ''
    trusted-users = hydra hydra-evaluator hydra-queue-runner
  '';

  nix.buildMachines = [
    {
      hostName = "localhost";
      systems = [ "i686-linux" "x86_64-linux" ];
      maxJobs = 6;
    }
  ];
```

At this point, if we've done a `nixos rebuild switch`, we should be able to see the Hydra web interface running at http://localhost:3000.
Our excitement will be short-lived, however, since we don't have any users configured for the web interface.

### Setting up a hydra admin

We have to use a command-line tool to set up a Hydra admin user:
```
> sudo su - hydra
> hydra-create-user alice --full-name 'Alice Q. User' --email-address 'alice@example.org' --password foobar --role admin
```

The admin user can add other users, but the above command is how we get the ball rolling.

## Adding projects

TODO projects, jobsets and jobs
TODO evaluations and builds

## Using `hydra` as a client

Getting changes into Hydra is just a matter of pushing to the relevant git repository.

If you have Hydra configured to send committers emails, then you'll get updates as the job progresses.
If not, you can watch the progress of the job through the web interface.

Once the build has finished successfully, you have a few choices for getting hold of the outputs.
In either case, Hydra has good instructions on what to do at the relevant places in the web interface.

If you are grabbing the outputs from the page for a particular build, the information that you need is provided by the "Help" buttons.

If you want to subscribe to a `Nix` channel so that you install and update packages from a project as and when you like, the instructions for that will be on the "Channel" page under the "Project" tab.

In either case, setting up your local Nix environment to use the binary cache provided by Hydra will most likely save you a lot of work.

### Using the binary caches

There are different ways to set up Nix to use the binary cache provided by Hydra, depending on whether or not we are running NixOS or not.

On NixOS, we add something like the following to `/etc/nix/configuration.nix`:
```nix
  nix.binaryCaches = [ 
    "http://localhost:3000"
    ...
  ];
  nix.binaryCachePublicKeys = [ 
    "hydra.localhost:8uy1Yef99zFBVLgDuXqSW7WFnZbwPSHLGrvrJkSmfNc="
    ...
  ];
``` 

On other platforms, we instead make the following addition to `/etc/nix/nix.conf`:
```nix
binary-caches = http://localhost:3000 ...
binary-cache-public-keys = hydra.localhost:8uy1Yef99zFBVLgDuXqSW7WFnZbwPSHLGrvrJkSmfNc= ...
```

If you want some caches to be available but not to be used by default, you can use `trustedBinaryCaches` in place of `binaryCaches`.
With that set, you need to pass some extra commands to use the caches, but the keys are already set up for you to use.

TODO example

## Example: A Haskell project

### Nixifying a cabal project

We need to set up a `default.nix`, which we can do using `cabal2nix`:

```
cd <project directory>
cabal2nix . > default.nix
```

If we want to play around with it in `nix-shell`, we should set up a `shell.nix`.

I usually use
```
cabal2nix --shell . > shell.nix
```
and then modify the result to refer to the `default.nix` we generated before.

We end up with something like:
```nix
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  # We could name the nix file we are referring to, but
  # the thing we generated before is called `default.nix`
  # for a reason.
  drv = haskellPackages.callPackage ./. {};

in

  if pkgs.lib.inNixShell then drv.env else drv
```

### Building a package

We need to create a Nix expression that describes the jobs we want to run for our package.

For now we just want to run one job - the one that builds the package - and that job has quite lot in common with our shell.nix.

The convention is to call this file `release.nix`:
```nix
let
  pkgs = import <nixpkgs> ( {
  } );
  version = "0.0.0.1";
  ghc = pkgs.haskellPackages;
in
rec {
  test-hs =
    ghc.callPackage ./. {};
}
```

This defines a jobset with one job in it - called `test-hs`.
The output will include any libraries or executables described in the `.cabal` file, along with any associated Haddock documentation.

If the `.cabal` file contains test suites these tests will be run as part of the build, and the build will fail if the tests fail.
There won't be any test reports generated, but their output will be visible in the build logs.

We can build this locally to test things out with:
```
nix-build -I . release.nix -A test-hs
```
and if we had multiple jobs in the jobset we could change the `-A` parameter to each of the jobs that we wanted to build.

### Adding test reports to the output

## Example: A Hakyll blog

### A Hakyll quickstart

[Hakyll](https://jaspervdj.be/hakyll/) is a website generator written in Haskell.

If you install `hakyll`, running 

```
hakyll init <dir>
``` 

will give set up an initial demo website.

This will include: 

- a Haskell file called `site.hs`, which generates the `site` executable that builds the website
- a cabal file to build `site`
- various example web pages, blog posts and web assets

You can run `site watch` to have your blog posts and other assets auto-compiled when they change and served on a preview server.
Once you are happy with what you have, you can run `site build` to build the website.

### Building the `site` executable

This is pretty straightforward - we are building an executable from a Haskell project as we did before:
```
let
  pkgs = import <nixpkgs> {};

  site = pkgs.haskellPackages.callPackage ./. {};

  jobs = rec {
    site = site
  };
in
  jobs
```

### Building the blog assets using the `site` executable

We could generate another `nix` derivation that depends on `site` to generate the webpages.

Instead with use `overrideCabal` to alter the build steps for the `site` derivation.

We want to make use of the `site` executable, so we do this by customizing the `postInstall` step of `site`:
```
let
  pkgs = import <nixpkgs> {};

  site = pkgs.haskellPackages.callPackage ./. {};

  jobs = rec {

    blog = pkgs.haskell.lib.overrideCabal site (drv: {
      postInstall = ''
        $out/bin/site build
        mkdir -p $out/blog
        cp -r _site/* $out/blog
        rm -Rf $out/bin
      '';
    });

  };
in
  jobs
```

If we run this locally with `nix-build -I . release.nix -A blog`, we'll end up with the contents of the blog in `result/blog`.

## Continuously deploying the blog using `hail`

TODO different steps for an actual service vs something like a blog
