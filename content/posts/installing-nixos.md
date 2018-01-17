---
title: NixOS install guide
date: 2018-01-17
project: infra
authors: ajmccluskey
---

If you're not used to the arcana of hands on Linux installations, then installing NixOS might be a
little intimidating. The [official documentation](https://nixos.org/nixos/manual/) does a good job
of explaining what needs to be done, but you might have to piece different sections together to get
the job done. Our team all currently run NixOS on their work machines, so we have an install guide
that provides a more step-by-step experience. It's been tested a couple of times now, so I thought
we should make it public.

## Conventions

I'll be using `$` for regular user login prompts, `#` for root login prompts, and `--` for comments.

## Requirements

Our team have some requirements for security and as a result of the hardware we use. They are:

- LVM disk partitioning
- [Luks encryption](https://en.wikipedia.org/wiki/Linux_Unified_Key_Setup) for the entire LVM
  physical volume
- UEFI boot loader and boot partition
- USB stick install

We don't dual boot, so this guide will only cover a full disk installation.

## Installation media

Firstly, we need to get the appropriate install media. We use the 64-bit minimal install CD. This,
and others, are available on the [NixOS downloads page](https://nixos.org/nixos/download.html).

Once you've obtained the installation media, copy it to a USB stick. **Note that this will destroy
anything on the USB stick**. We'll assume that `/dev/sda` is the device you want to copy to.

```
$ lsblk
-- identify that /dev/sda is the USB stick we want the installer on

# dd if=$INSTALLER_ISO of=/dev/sda bs=1M
-- no output until it's finished
```

## References

Here are the sources I consulted in putting this together.

- [NixOS Manual](https://nixos.org/nixos/manual/)
- [bluishcoder install guide](https://bluishcoder.co.nz/2014/05/14/installing-nixos-with-encrypted-root-on-thinkpad-w540.html)

