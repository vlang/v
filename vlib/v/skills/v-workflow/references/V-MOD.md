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
		'vlang.markdown'
		'vweb.vdom'
	]
}
```

Only `name` and `dependencies` matter to the compiler. The rest are metadata.

## Where an import is looked up

For `import vweb.vdom`, in order:

1. `~/.vmodules/vweb/vdom/` — installed with `v install`
2. `VMODULES`, if that environment variable is set
3. `.vmodules/` inside the project — installed with `v link`
4. `vlib/` of the V source tree, for the standard library

The first three are yours to manage. `vlib/` belongs to the compiler.

## The three commands

```bash
v install vweb.vdom    # copy into ~/.vmodules, for every project of this user
v link                  # symlink the current project into ~/.vmodules
v unlink                # remove that symlink
```

`v link` is for **your own** project while you work on it, so that another project
can import it by name. `v unlink` before you push, or the next person will get a
symlink pointing at your machine.

```bash
v list                 # what is installed
v outdated             # what has a newer version
v upgrade              # update them
```

## When a dependency will not resolve

In order of likelihood:

1. **Not installed.** `v list` is empty for it. Run `v install`.
2. **A stale link.** A `v link` pointing at a directory that has moved. `v unlink`
   and link again.
3. **The module name does not match its directory.** The `module` line must equal
   the directory name, exactly. A mismatch imports cleanly and then finds nothing.
4. **A typo in the version or the name.** The registry name is lowercase and
   hyphenated.

When a dependency is not found the error names the module and stops. That is
almost always `v install`, not a code problem — do not go looking through your
imports.

## Adding a dependency

Prefer declaring it in `v.mod` and running `v install`, rather than installing
into the user's home directory by hand. A dependency the repository does not
record is a dependency the next person cannot build.

```bash
v -o ~/.vmodules/ vmod init   # no: just edit v.mod
```

Edit `v.mod`, then:

```bash
v install vweb.vdom
```

## The V repository

The V source tree is itself a `v.mod` project, named `V`. Working inside it means:

- `v install` for a tool the repo does not vendor
- `./v self` after touching `vlib/v/` or `cmd/v/`
- `./v -check` for library code needs `-shared`

The last point catches people: `v -check vlib/v/skills/` fails with *"project must
include a `main` module"* because the target is a library. Add `-shared`.

## Locking

There is no lock file. `v.mod` records versions as ranges, so two people can resolve
to different revisions of the same dependency. When that matters, pin the exact
revision in `v.mod` and check the diff into the repository.