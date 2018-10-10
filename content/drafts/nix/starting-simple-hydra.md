---
title: Your First Hydra
date: 2018-10-10
authors: schalmers
project: infra
---

So you've started using [`Nix`](https://nixos.org/nix) for your projects. The reproducible builds
and dependency management are predictable and declarative. For the first time in a long time the
ground at your feet feels firm, you're no longer treading quicksand. You size up your nemesis, the
Continuous Integration server, long has it defied your attempts to build as you do... No longer.

This post is a guide for setting up a small local instance of the `Nix` powered CI system known as [Hydra](https://nixos.org/hydra/):

<div class="card mb-2"><div class="card-body"><p><i class="fa fa-quote-left fa-fw fa-lg"></i>
Hydra is a Nix-based continuous build system, released under the terms of the GNU GPLv3 or (at your
option) any later version. It continuously checks out sources of software projects from version
management systems to build, test and release them. The build tasks are described using Nix
expressions. This allows a Hydra build task to specify all the dependencies needed to build or test
a project. It supports a number of operating systems, such as various GNU/Linux flavours, Mac OS X,
and Windows.
<i class="fa fa-quote-right fa-fw fa-lg"></i></p><p class="mb-0">
--- [Hydra Homepage](https://nixos.org/hydra/)
</p></div></div>

Hydra is a very powerful system, and like most powerful systems it can be difficult to know where to
start. This post aims to help by walking through the process of setting up a local Hydra instance on
a virtual machine. This will be a stand-alone Hydra instance, only a single machine without build
slaves.

'Why did it need to have eight heads?', you ponder briefly before deciding you'd rather not know...

### Requirements

This tutorial was tested on a machine running `NixOS`, which is not required, but you _will_ need:

- [`Nix`](https://nixos.org/nix),
- [`NixPkgs`](https://nixos.org/nixpkgs), 
- and [`NixOps`](https://nixos.org/nixops). 

To install `nixops`:
```
$ nix-env -i nixops
```

You should be comfortable writing your own `Nix` expressions and debugging failures. This is still
somewhat of a bleeding edge part of the `Nix` ecosystem, so things don't always go according to plan.

### NixOps

To manage the Hydra machine, we're going to leverage `NixOps` to create, start, run, and update our
virtual machine. This lets us have a declarative defintion of our build machine, as well as a
consistent interface for managing the machine(s).

We're going to create two Nix expressions that together define our machine:

1.  The virtual machine environment: `simple_hydra_vbox.nix`
2.  The configuration of the Hydra machine: `simple_hydra.nix`

### The virtual machine

This will be a 'headless' VirtualBox machine, with some settings to keep the overall size of the
machine down, and control access.

We must create a `Nix` `attrset` that contains a function that defines our machine, this function is
attached to a name that will be used as the `<HOSTNAME>` of the machine within `NixOps`. We use the
name `my-hydra` here, and we'll need to use that name again when we write our machine configuration.

    # START simple_hydra_vbox.nix
    {
      my-hydra =
        { config, pkgs, ... }: {

`NixOps` lets you choose your deployment target, ours being VirtualBox. `NixOps`
supports quite a few different environemnts. Check out the [NixOps Manual](https://nixos.org/nixops/manual/) 
for more information on supported target environments.

          deployment.targetEnv = "virtualbox**;

Click here to find out more about why `deployment` and similar attributes are used this way:
<a class="btn btn-info" role="button" data-toggle="collapse" href="#attrsetAside" aria-expanded="false" aria-controls="attrsetAside">
Combining *_attrset_* values
</a>

<div class="collapse mb-3" id="attrsetAside"><div class="card card-body">
Note that `deployment` is not passed in as an argument to our configuration. This is intentional
and forms part of an idiomatic `Nix` technique for building and overriding attributes. We build a
set of attributes with a specific predefined structure, such that `NixOps` can union them together,
then act upon those settings to create the machines we've specified.

As an example, say we have some "_default_" settings in an `attrset` somewhere:

    defaultAttrs = { fieldA = "generic_name";
    , fieldB = "foo";
    , fieldC = 3000;
    }

To override only `fieldB`, we create the following `attrset` in our expression:

    myAttrs = { fieldB = "bar"; }

Then when the `attrset` values are combined, the original _default_ values can be overridden like so:

    # (//) is a built in Nix function to union two attrsets together
    defaultAttrs // myAttrs
    =
    { fieldA = "generic_name";
    , fieldB = "bar";
    , fieldC = 3000;
    }

The ordering of the union matters, `myAttrs // defaultAttrs` would override the values in the other
direction.

This is action is recursive, nested properties are combined as part of this process.

    ({ fieldA = 3000; fieldB = { nestedA = "foo"; }; }) // ({ fieldB.nestedA = "bar"; })
    =
    { fieldA = 3000; fieldB = { nestedA = "bar"; }; }

This capability allows you to have all of the settings you want and you need only ever specify the
precise ones you need to alter. This can then be combined with other `attrset`s that alter other
settings for a long chain of overrides to end up with a final set. Such as this contrived example:

    defaultMachineConf 
    // ({ machine.cpus = 2; })          # "hardware" settings in one file
    // ({ machine.os.user = "Sally"; }) # "software" settings in a different file

</div></div>

Next we tweak the settings for our VirtualBox machine. The machine will need a fair chunk of memory,
as evaluating the `nixpkgs` tree to build the dependencies can be expensive. Also, as with any CI
system, you need to make sure your target has sufficient resources to actually run the required builds.

          deployment.virtualbox.memorySize = 4096;
          deployment.virtualbox.vcpu = 2;

Our machine doesn't require a GUI so we enable `headless` mode. Note that these are `virtualbox`
specific settings, each target will have their own collection of options, check the manual for more
information.

          deployment.virtualbox.headless = true;

We'll also turn off the manual from appearing on the machine, this saves a bit more space. Enable
a time daemon, `ntp`. Then set the access controls for SSH, disallowing SFTP and password based
authentication.

          services.nixosManual.showManual         = false;
          services.ntp.enable                     = false;
          services.openssh.allowSFTP              = false;
          services.openssh.passwordAuthentication = false;

Provide some settings for the users on the machine. The `mutableUsers` option means that every time
the system is deployed the user information will be completely replaced by the static definition
from this file. The next setting for `keyFiles` will copy the listed SSH key files to the target
machine. These files are expected to exist on the machine where the `nixops` command is being
executed.

In this example we're going to copy across an RSA public key from a pair. This allows us to use
`nixops ssh -d <deployment-name> <machine-name>` to access the machine via SSH.

          users = {
            mutableUsers = false;
            users.root.openssh.authorizedKeys.keyFiles = [ ~/.ssh/id_rsa.pub ];
          };
        };
    }
    # END: simple_hydra_vbox.nix

We put all of the above in a file named `simple_hydra_vbox.nix`, this will form part of the input
for our `nixops` command later on. This is the machine, but that isn't much good without a
definition for what will actually be _running_ on the machine. 

Next we will define the `NixOS` configuration that will be running on our virtual machine. This will
include our Hydra service, as well as its dependencies, with some caveats because we're running on a
local, virtualised machine.

### A basic Hydra

What follows is a `NixOps` expression for a stand-alone Hydra build machine. It is enough to start
your own Hydra instance with a binary cache. There are sections which are required for running this
machine in a virtual machine, but we'll cover those as we go. This machine will not define any build
slaves, so it will build and cache everything locally.

As before, we write a `nix` expression that is a function that produces an `attrset` that is our
machine configuration. Note that the name we assign matches the virualisation configuration. This
must match otherwise there will be an error from `NixOps`. This attribute is also used as the
machine hostname, so any where you see `<HOSTNAME>` in the configuration, replace that with this
name.

    # START: simple_hydra.nix
    {
      my-hydra = 
        { config, pkgs, ...}: {

As Hydra is able to send emails for various events, so our machine will need some sort of email
capability. We enable the `postfix` service like so:

    services.postfix = {
        enable = true;
        setSendmail = true;
    };

The build queues and parts of the jobset configuration are all stored in a postgres database on the
Hydra machine, so we need to enable the `postgresql` service, and provide a mapping from `NixOS`
users to `PostgreSQL` users. Otherwise Hydra won't be able to access the database!

    services.postgresql = {
        enable = true;
        package = pkgs.postgresql;
        identMap =
          ''
            hydra-users hydra hydra
            hydra-users hydra-queue-runner hydra
            hydra-users hydra-www hydra
            hydra-users root postgres
            hydra-users postgres postgres
          '';
    };


This next setting is to tell `NixOS` to open the TCP port for accessing the web interface for Hydra.
We don't need to add the `postfix` or `ssh` ports in this instance because the `postfix` and
`virtualisation` configuration will add them on our behalf.

```
    networking.firewall.allowedTCPPorts = [ config.services.hydra.port ];
```

Now onto the meaty part of creating our Hydra configuration, enabling and configuring the `hydra`
service.

    services.hydra = {
        enable = true;
        useSubstitutes = true;
        hydraURL = "https://hydra.example.org";
        notificationSender = "hydra@example.org";

The `hydraURL` setting is the page for the Hydra web interface, it is also used in emails as the
root path for any links back to this Hydra system. The `notificationSender` setting is the **`from`**
address of emails from this Hydra system.

The `useSubstitutes` setting is a bit more complicated, it's best to refer to the manual for this one:

<div class="card mb-2"><div class="card-body"><p>
<i class="fa fa-quote-left fa-fw fa-lg"></i>
Whether to use binary caches for downloading store paths. Note that binary substitutions trigger (a
potentially large number of) additional HTTP requests that slow down the queue monitor thread
significantly. Also, this Hydra instance will serve those downloaded store paths to its users with
its own signature attached as if it had built them itself, so don't enable this feature unless your
active binary caches are absolute trustworthy.
<i class="fa fa-quote-right fa-fw fa-lg"></i></p>
<p class="mb-0">--- From <https://nixos.org/nixos/options.html#services.hydra.usesubstitutes></p>
</div></div>

With this enabled you will reap the benefits of cached build dependencies, and spare precious cycles
by not constantly rebuilding things unnecessarily. The trade-off is a higher number of HTTP requests
that may affect the running of the Hydra system. Depending on how active your build queue is, this
may be a cause for concern.

The following setting is important and required (even though it is empty). Because we're building a
stand-alone Hydra instance, we have to explicitly tell Hydra that there are no additional build
machines.

Without this setting, Hydra will ignore our `machine` definition and the Hydra instance will be not
function, as the expected build machine configuration does not match reality.

    buildMachinesFiles = [];

We provide some `extraConfig` to tell Hydra where to place the binary cache and its secret key.

        extraConfig = ''
          store_uri = file:///var/lib/hydra/cache?secret-key=/etc/nix/<HOSTNAME>/secret
          binary_cache_secret_key_file = /etc/nix/<HOSTNAME>/secret
          binary_cache_dir = /var/lib/hydra/cache
        '';
    };

See here (<https://nixos.org/nixos/options.html#services.hydra>) for a full list of the options
available for the Hydra service in `NixOS`.

The following is optional, but here for example purposes about using `nginx` to serve the Hydra web
interface. This Hydra instance was running locally in a VM so this isn't needed. The full scope of
Nginx configuration and what is available via `NixOS` is well beyond this post.

    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      virtualHosts."hydra.example.org" = {
        forceSSL = true;
        enableACME = true;
        locations."/".proxyPass = "http://localhost:3000";
      };
    };

Hydra requires manual steps beyond the configuration of the `NixOS` service. This is only a one time
requirement however. We leverage a `systemd` service to manage the timing. This provides some
additional guarantees that the final configuration will run _after_ Hydra is installed and running.

The initial options describe our `systemd` service.

    systemd.services.hydra-manual-setup = {
      description = "Create Admin User for Hydra";
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      wantedBy = [ "multi-user.target" ];
      requires = [ "hydra-init.service" ];
      after = [ "hydra-init.service" ];
      environment = builtins.removeAttrs (config.systemd.services.hydra-init.environment) ["PATH"];

This is the script that is run by the `systemd` service, we predicate its execution on the lack of
existence of a `.setup-is-complete` file.

      script = ''
        if [ ! -e ~hydra/.setup-is-complete ]; then
          # create signing keys
          /run/current-system/sw/bin/install -d -m 551 /etc/nix/<HOSTNAME>
          /run/current-system/sw/bin/nix-store --generate-binary-cache-key <HOSTNAME> /etc/nix/<HOSTNAME>/secret /etc/nix/<HOSTNAME>/public
          /run/current-system/sw/bin/chown -R hydra:hydra /etc/nix/<HOSTNAME>
          /run/current-system/sw/bin/chmod 440 /etc/nix/<HOSTNAME>/secret
          /run/current-system/sw/bin/chmod 444 /etc/nix/<HOSTNAME>/public
          # create cache
          /run/current-system/sw/bin/install -d -m 755 /var/lib/hydra/cache
          /run/current-system/sw/bin/chown -R hydra-queue-runner:hydra /var/lib/hydra/cache
          # done
          touch ~hydra/.setup-is-complete
        fi
      '';
    };

As we indicated in the `extraConfig` section of Hydra, we're going to have a binary store on this
machine. This is where we generate the secret keys and create the cache itself.

Next we set some explict `nix` settings to schedule the garbage collection of the store, and also
the automatically optimise the store. Which means that `nix` will remove duplicate files from the
store, replacing them with hard links to a single copy.

    nix.gc = {
      automatic = true;
      dates = "15 3 * * *"; # [1]
    };
    
    nix.autoOptimiseStore = true;

\[1\]: This is a [CRON](https://crontab.guru/#15_3_*_*_*) schedule. To find out more you can read the [`crontab` manual](http://man7.org/linux/man-pages/man5/crontab.5.html) or the [Wikipedia page](https://en.wikipedia.org/wiki/Cron).

This tells `NixOS` which users have privileged access to the `nix` store. These users can add
additional binary caches.

    nix.trustedUsers = ["hydra" "hydra-evaluator" "hydra-queue-runner"];

Lastly, we define a build machine so that Hydra has a 'builder' to orchestrate. In this example
we're building locally so our `hostName` is "localhost". Use the `systems` setting to indicate the
systems or architectures that we able to build for, and `maxJobs` to set the maximum number of build
processes.

The `supportedFeatures` property is a list of "Features" that this build machine supports. If a
derivation is submitted that requires features that are not supported on a given build machine then
it will not be built. 

        nix.buildMachines = [
          {
            hostName = "localhost";
            systems = [ "x86_64-linux" "i686-linux" ];
            maxJobs = 6;
            # for building VirtualBox VMs as build artifacts, you might need other 
            # features depending on what you are doing
            supportedFeatures = [ ];
          }
        ];
      };
    }
    # END: simple_hydra.nix

Place all of that configuration in a file called `simple_hydra.nix` and we're ready to start powering up.

#### Powering up

Now to tell `NixOps` to create and prepare our Hydra instance.

    $ cd <path to 'simple_hydra' nix files>
    $ ls
    simple_hydra.nix simple_hydra_vbox.nix

##### Create the virtual machine within NixOps

Before you can initiate the deployment of your machine, you need to make `NixOps` aware of it:

    $ nixops create ./simple_hydra.nix ./simple_hydra_vbox.nix -d simple_hydra
    created deployment ‘5537e9ba-cabf-11e8-80d0-d481d7517abf’ ### This hash will differ for your machine
    5537e9ba-cabf-11e8-80d0-d481d7517abf

If that completes without error, you can then issue the following command to `NixOps`:

    $ nixops info -d simple_hydra
    Network name: simple_hydra
    Network UUID: 5537e9ba-cabf-11e8-80d0-d481d7517abf
    Network description: Unnamed NixOps network
    Nix expressions: /path/to/simple_hydra.nix /path/to/simple_hydra_vbox.nix
    
    +----------|---------------|------------|-------------|------------+
    | Name     |     Status    | Type       | Resource Id | IP address |
    +----------|---------------|------------|-------------|------------+
    | my-hydra | Missing / New | virtualbox |             |            |
    +----------|---------------|------------|-------------|------------+

The `info` command for `nixops` provides feedback about the status of the machine(s) you have
deployed, their IP addresses, and other things. This information is stored local to the machine that
issued this `nixops` command, it is not shared between different deployment machines. So even if you
deploy to AWS using `nixops`, someone else that has the same `nix` expressions for your machines
will not have any knowledge of their deployment status.

##### Trigger deployment of the virtual machine

We can now instruct `nixops` to deploy the configuration to the virtual machines, this part may take
a while. Once you issue the following command you might like to get up, go for a walk, stretch, or
fetch a cup of your favourite beverage.

The `--force-reboot` is required to ensure the Hydra services are started correctly. If you do not
include it then the deployment phase will complete successfully, but it will report an error as not
all of the services will have been initiated correctly. 

This flag is also required to ensure everything ticks over as required, as there are some `nixops`
errors that can be avoided or resolved by simply power cycling the machine.

The `-d` flag that we give to `nixops` is to specify the name of the deployment we want to use. If
you don't provide this information then `nixops` will return this error:

    error: state file contains multiple deployments, so you should specify which one to use using ‘-d’, or set the environment variable NIXOPS_DEPLOYMENT

You can set the environment variable `NIXOPS_DEPLOYMENT` to `simple_hydra` if you don't want to
include the `-d simple_hydra` information every time. 

    $ nixops deploy -d simple_hydra --force-reboot

`NixOps` will then set about the task of creating the virtual machine, initialising all the disks
and network settings etc. Then create the configuration prior to copying to the newly minted
machine. After which, barring any errors you should see the following text:

    my-hydra> waiting for the machine to finish rebooting...[down]......................[up]
    my-hydra> activation finished successfully
    simple_hydra> deployment finished successfully

Once the machine is deployed, you can check its status using `$ nixops info -d simple_hydra`.

    Network name: simple_hydra
    Network UUID: c58245b3-cac2-11e8-9346-d481d7517abf
    Network description: Unnamed NixOps network
    Nix expressions: /path/to/simple_hydra.nix /path/to/simple_hydra_vbox.nix
    
    +----------|-----------------|------------|------------------------------------------------------|----------------+
    | Name     |      Status     | Type       | Resource Id                                          | IP address     |
    +----------|-----------------|------------|------------------------------------------------------|----------------+
    | my-hydra | Up / Up-to-date | virtualbox | nixops-c58245b3-cac2-11e8-9346-d481d7517abf-my-hydra | 192.168.56.101 |
    +----------|-----------------|------------|------------------------------------------------------|----------------+

This deployment depends on the files containing the nix expressions that were used to create it, if
those files disappear then you will not be able to destroy or update the machine via `NixOps`. 

If you want to make changes to these machines or configuration, such as adding a CPU, or changing
the port for the Hydra web UI. You make the change in those files, then you only have to issue the
`deploy` command again and `NixOps` will take care of updating the machine to the new configuration.

If things have gone according to plan then you should be able to point a browser at the Hydra web
UI. The address will be the default Hydra port of `3000` at the IP address listed in the output from
the `nixops info -d simple_hydra` command. Using the above output as an example the address for the
Hydra page is: `http://192.168.56.101:3000`.

We're ready to start adding some `jobsets` to our Hydra instance, but there is a bit of a problem as
we can't login to our Hydra UI.

### Add the Hydra admin user

To add an admin user to Hydra, we use the `hydra-create-user` program that is included with Hydra.
This must be run on the Hydra machine itself, so we need to SSH in or send the instruction over SSH.

The `nixops ssh` command lets you do both:

To run the command on the machine by connecting first, run the following:

    $ nixops ssh -d simple_hydra my-hydra

This will put you on the machine at a prompt as `root` on the `my-hydra` machine from the
`simple_hydra` deployment. You can then run the create user command.

Alternatively you can simply send the single instruction over SSH:

    $ nixops ssh -d simple_hydra my-hydra '<create-user-command>'
    creating new user `admin`

Putting the command in quotes after the `nixops ssh` command will send a single instruction over
SSH, printing any output.

The command to create an admin user is:

    $ hydra-create-user admin \
        --full-name "Bobby Admin" \
        --email-address "bobby@example.org" \
        --password TOTALLYSECURE \
        --role admin

With successful output being something like:

    creating new user `admin`

You can now use these credentials to login to your Hydra web UI and start configuring your jobsets
and other Hydra settings. We plan to cover more information about writing your own declarative
jobsets in a future post.

For the impatient or curious, you can have a look at the setup we use at QFPL for some of our projects:

* [Waargonaut](https://github.com/qfpl/waargonaut/tree/master/ci)
* [Digit](https://github.com/qfpl/digit/tree/master/ci)
* [SV](https://github.com/qfpl/sv/tree/master/ci)

### One prepared earlier

If you're looking at most of the code in this post and thinking that all it would take is some
simple inputs and interpolation then you wouldn't need to write the same `nix` expressions every
time. You would be correct, and a kindly soul by the Github handle of `ElvishJerricco` has done
just that in [`simple-hydra`](https://github.com/ElvishJerricco/simple-hydra). This repo has a
version of what we have worked through here, but abstracted so that you need only specify some
choices along with hostnames etc.

For the sake of your own understanding, it is useful to work through it yourself first so you can
more readily experiment and learn about the inner workings.

### Going further

For those looking for more advanced Hydra information, then the [Hydra Tutorial](https://github.com/peti/hydra-tutorial)
by Github user `peti` is an excellent resource. The tutorial is also worked through as part of a
presentation linked to from the repo: ([presentation link](https://www.youtube.com/watch?v=RXV0Y5Bn-QQ)).

That tutorial sets up a more complex Hydra configuration with a 'master' machine and 'slave'
machines, in what would more closely resemble a real world setup.
