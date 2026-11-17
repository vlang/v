# The v.mod module system

> The commands are in the parent skill. This reference covers: the file format,
> where an import is looked up, and why a dependency sometimes will not resolve.

## The file

A project has exactly one `v.mod`, at its root. It is not TOML and not JSON; it
is V:

```v ignore
Module {
	name: 'my-project'
	description: 'What it does.'
	version: '0.1.0'
	license: 'MIT'
	repo_url: 'https://github.com/me/my-project'
	dependencies: [
		'markdown'
		'vsl@^0.1.47'
	]
}
```

A dependency is a package name, optionally followed by `@` and a version range
(`^0.1.47`, `>=0.4.0 <0.6.0`) or an exact Git ref (`@v1.2.3`). A bare name follows
the package's default branch. vpm also reads `dev_dependencies`,
`dependency_overrides`, `min_v` and `retracted`; `cmd/tools/vpm/README.md` has the
details. The folder holding `v.mod` is the root that the project's imports resolve
against.

## Where an import is looked up

For `import foo.bar`, in order:

1. The project root, the folder holding the nearest `v.mod`: `foo/bar/` there.
   This includes packages installed with `v install --local`.
2. `foo/bar/` beside the importing file.
3. `vlib/` of the V source tree, for the standard library.
4. The module directory: `~/.vmodules`, or the directories listed in `VMODULES`,
   which replaces `~/.vmodules` rather than adding to it. `v install` and `v link`
   put packages here.
5. The parent folders of the importing file, so two sibling checkouts can import
   each other without being installed.

That is the default. `-path` replaces steps 3 and 4 with its own `|`-separated
list, in which `@vlib` and `@vmodules` stand for those two. The virtual
`modules/` directory is no longer searched; the compiler says so, and
prints the move command, when a module is only found there.

## The three commands

```bash
v install            # install the dependencies declared in v.mod, and lock them
v link               # symlink the current project into the module directory
v unlink             # remove that symlink
```

`v install <package>` with a package name installs it into the module directory
for every project of this user, and does **not** touch `v.mod` or `v.mod.lock`.

`v link` is for **your own** project while you work on it, so that another project
can import it by name. The symlink lives in the module directory, not in the
repository.

```bash
v list                 # what is installed
v outdated             # what has a newer version
v upgrade              # update them
v why <package>        # which dependency pulled it in
v mod graph            # the dependency edges, offline
```

## When a dependency will not resolve

In order of likelihood:

1. **Not installed.** `v list` does not show it. Run `v install` in the project.
2. **A stale link.** A `v link` pointing at a directory that has moved. `v unlink`
   and link again.
3. **The module name does not match its directory.** The `module` line must equal
   the directory name, exactly. A mismatch is reported as *"bad module
   definition"*, naming both the imported and the declared module.
4. **The import path is not the package name.** `-` becomes `_` and the name is
   lowercased: the package `my-mod` is imported as `my_mod`. `v install` warns when
   it normalizes a name.
5. **No release satisfies the range.** Installation fails and names the requirement
   chains. Widen the range in `v.mod`, or relax the requirement that conflicts.

When a dependency is not found the error names the module and stops. That is
almost always `v install`, not a code problem — do not go looking through your
imports.

## Adding a dependency

Declare it in `v.mod`, then run `v install` without arguments in the project. A
dependency the repository does not record is a dependency the next person cannot
build, and `v install <package>` alone records nothing.

```bash
# after adding 'vsl@^0.1.47' to the dependencies in v.mod
v install
```

## The V repository

The V source tree is itself a `v.mod` project, named `V`. Working inside it means:

- `v install` for a tool the repo does not vendor
- `./v self` after touching `vlib/v/` or `cmd/v/`
- `./v -check` for library code needs `-shared`

The last point catches people: `v -check vlib/v/skills/` fails with *"project must
include a `main` module or be a shared library"* because the target is a library.
Add `-shared`.

## Locking

`v install` run in a project, without package names, records what it resolved in
`v.mod.lock`, next to `v.mod`: for each dependency, the requested string, the
selected version, the commit SHA, the source URL and a content hash. Commit that
file. Later installs reuse the locked commits, even when newer tags exist.

```bash
v install --locked     # fail instead of resolving anything differently from the lock
v install --frozen     # the same, and never write v.mod.lock
v update               # move dependencies forward and rewrite their lock entries
v remove <package>     # also drops its lock entry
```

`v install --locked` is the check for CI. The global module directory holds one
checkout per package, shared by every project; `v install --local` gives a project
checkouts of its own.
