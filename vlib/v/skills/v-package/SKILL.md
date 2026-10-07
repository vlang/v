---
name: v-package
description: "Manage V packages: install, update, search, remove and link local modules."
license: MIT
---

# V packages with vpm

Use this when adding dependencies, resolving a missing package, or working with
`v install`, `v update`, `v search` or `v link`. For building and testing, see
`v-workflow`; for language rules, see `v-lang`.

Invoke package commands as `v <subcommand>`, such as `v install <module>`.
The frontend dispatches these commands to the `vpm` tool; `v pm` is not a package
subcommand.

```bash
v install <module>   # install a package
v update             # update the installed packages
v outdated           # which installed packages have a newer version
v list               # what is installed
v remove <module>    # remove a package
v show <module>      # information about a package
v search <keyword>   # search vpm.vlang.io
v why <module>       # why a module is in the dependency graph
v link               # symlink the current project into the modules directory
v unlink             # remove that symlink
v upgrade            # upgrade all outdated modules
```

## Where modules go

`~/.vmodules`, or `$VMODULES` when it is set. A module is stored under its name
with `.` replaced by the path separator, so `prantlf.json` becomes
`~/.vmodules/prantlf/json`.

## Installing

`v install <module>` installs a package into the modules directory. It does not
add the package to the project's `v.mod`; add the dependency there explicitly.
Run `v install` without package arguments from the project directory to install
the dependencies already declared in `v.mod`.

Git packages include their file contents in the initial clone, so checkout can
complete without a second network request for missing file blobs. Submodules are
installed recursively.

## Removing

`v remove <module>` refuses two cases rather than guessing: a module outside the
modules directory, and one that was not installed by vpm. For a linked module the
answer is `v unlink` first.

## Linking a local module

`v link` symlinks the current project into the modules directory, so a change
there is seen without reinstalling. `v unlink` removes the symlink.

## Resource Routing

- `references/SUBCOMMANDS.md` - Read when a subcommand is missing, ignored, or
  behaves differently from this summary.

## Quick Reference

```bash
v install <module>
v update
v outdated
v list
v remove <module>
v show <module>
v search <keyword>
v why <module>
v link
v unlink
v upgrade
```
