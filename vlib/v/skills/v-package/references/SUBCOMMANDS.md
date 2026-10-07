# The vpm subcommands

Read this when a subcommand is missing, ignored, or behaves differently from the
summary in `SKILL.md`.

## install

`v install <module>` clones the package into the modules directory and records it
in the project's `v.mod`. Run it in the project directory. With no `v.mod` present
it detects that and uses the directory as the module.

## update

`v update` updates the installed packages to their latest versions.

## upgrade

`v upgrade` upgrades all outdated modules. It is `v update` applied to everything
that has a newer version.

## outdated

`v outdated` lists the installed modules that have a newer version available.

## list

`v list` lists what is installed. With no modules installed it says so.

## remove

`v remove <module>` removes a package. It refuses a module outside the modules
directory, and one that was not installed by vpm, rather than guessing.

## show

`v show <module>` displays information about a module, whether it is installed or
not.

## search

`v search <keyword>` searches https://vpm.vlang.io/ for matching keywords and
displays the details.

## why

`v why <module>` explains why a module is in a project's dependency graph.

## link

`v link` symlinks the current project into the modules directory, so a change
there is seen without reinstalling. It reports whether the module is already
available or already linked.

## unlink

`v unlink` removes the current project's symlink from the modules directory. It
refuses a path that is not a symlink.

## Checking the installed set against the compiler

The subcommands are dispatched by the `v` frontend, which maps them to the `vpm`
tool. A compiler whose tree lacks a subcommand will not have it, so compare
`v skills list` against `ls vlib/v/skills` before concluding a subcommand is
missing upstream.