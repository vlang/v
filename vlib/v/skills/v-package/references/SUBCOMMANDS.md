# The vpm subcommands

Read this when a subcommand is missing, ignored, or behaves differently from the
summary in `SKILL.md`.

## install

`v install <module>` installs a package into the modules directory without
changing the project's `v.mod`. Add dependencies to that manifest explicitly.
`v install` without package arguments installs the dependencies declared in the
current directory's `v.mod`; it reports an error if that file is absent. That
project install records the resolved commits in `v.mod.lock`.

- `--locked` fails rather than resolve a dependency differently from the lock.
- `--frozen` does the same and never writes `v.mod.lock`.
- `--local` installs into the folder holding the project's `v.mod` instead of the
  global modules directory.
- `--once` skips a package that is already installed.

## update

`v update` updates installed packages. Project version ranges constrain the
selected releases, and a project update refreshes its `v.mod.lock`. `-p <module>`
updates one dependency, `--precise <ref>` picks its exact version, `--latest`
widens direct requirements to the newest release and rewrites `v.mod`, and
`--dry-run` only reports what would change.

## upgrade

`v upgrade` updates the modules reported as outdated by `v outdated`.

## outdated

`v outdated` lists the installed modules that have a newer version available.

## list

`v list` lists what is installed. With no modules installed it says so.

## remove

`v remove <module>` removes a package and, inside a project, drops its entry from
`v.mod.lock`. It refuses a module outside the modules directory, and one that was
not installed by vpm, rather than guessing. Without a module name it refuses to
run.

## show

`v show <module>` displays information about a module, whether it is installed or
not.

## search

`v search <keyword>` searches https://vpm.vlang.io/ for matching keywords and
displays the details.

## why

`v why <module>` explains why a module is in a project's dependency graph.

## vendor

`v vendor` copies the installed dependency graph, direct and transitive, into
`vendor/` at the project root, without querying any remote. Run it after
`v install`. It refuses to overwrite an existing `vendor/`. To build from the copy,
set `VMODULES` to the absolute path of that `vendor/` directory.

## link

`v link` symlinks the current project into the modules directory, so a change
there is seen without reinstalling. It reports whether the module is already
available or already linked.

## unlink

`v unlink` removes the current project's symlink from the modules directory. It
refuses a path that is not a symlink.

## Checking the installed set against the compiler

The subcommands are dispatched by the `v` frontend, which maps them to the `vpm`
tool. Read `v help vpm` for the installed compiler's package command help, or
`v help install` for installation options. A compiler whose tree lacks a
subcommand will not provide it; skill listings describe the installed skills,
not the package command surface.
