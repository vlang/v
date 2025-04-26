## Description

This tool generate prebuilt versions (or latest version by default) of:
1. TCC from git://repo.or.cz/tinycc.git
2. libgc from https://github.com/ivmai/bdwgc/.

## Pre-Install 

1. windows : `git`, `vs studio`(include `nmake`, `cl`)
2. nix : `git`, `automake`, `libtool`, `gmake`
3. ubuntu : `libgc-dev` 
3. freebsd : `boehm-gc-threaded-8.2.8`
4. openbsd : `boehm-gc`

## Usage

Simply run it:
```
v run updater.v
```

## Config

There is a config file `updater.toml`, it provide a easy way develop 
config for different platform/system.

1. [global] provide global config for the `updater`.
2. [tools_need] config tools need for different system(`nix` or `windows`).
3. [download] provide the git url and commit settings.
4. Then different platform/system configs. Such as [windows] is for `windows`, 
   [aarch64] is for `aarch64` platforms.

