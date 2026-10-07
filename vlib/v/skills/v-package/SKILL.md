---
name: v-package
description: How to install, update, search and remove V packages with `vpm`, and where they go. Use when a dependency will not resolve, when adding a package to a project, or when asked what `v install`, `v update`, `v search` or `v link` do. Covers the subcommands, the modules directory, and linking a local module. Does not cover the build loop (see v-workflow) or the language rules (see v-lang).
license: MIT
---

# V packages with vpm

The subcommands are `v <subcommand>`, not `v pm <subcommand>`. `pm` is the name of
the tool the frontend dispatches to, and typing it makes the frontend read it as a
second project name.

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

`v install <module>` clones the package into the modules directory and records it
in the project's `v.mod`. Git packages include their file contents in the initial
clone, so checkout can complete without a second network request for missing file
blobs, and submodules are installed recursively.

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