---
name: v-project
description: How to create and initialize a V project with `v new` and `v init`, and what each template produces. Use when starting a new project, scaffolding a module, or asked what `v new` or `v init` do. Covers the three templates, where their flags go, the v.mod prompt, and the resulting layout. Does not cover the build loop (see v-workflow), the language rules (see v-lang), or the wider command surface (see v-tools).
license: MIT
---

# Creating a V project

Two commands, one decision: `v new` makes a directory, `v init` works in the one
you are already in.

## The decision

- `v new <name>` — creates `<name>/` and puts the project in it.
- `v init` — puts the project in the current directory. Use this when the
  directory already exists.

Both take the same three template flags, and both create a `v.mod`.

## The flags go after `new`

This is the first thing that goes wrong. The template flags belong to `v new`,
so they come after it:

```bash
v new myapp           # executable, the default
v new mylib --lib     # library
v new myapp --web     # veb web app
```

`v new --lib myapp` fails with *"too many arguments"*: `--lib` is read as a
second project name.

## What each template makes

| Flag | Entry point | Module | Extra |
| --- | --- | --- | --- |
| `--bin` (default) | `main.v` | `main` | — |
| `--lib` | `<name>.v` | `<name>` | `tests/<fn>_test.v` |
| `--web` | `main.v` | `main` | `assets/main.css`, `templates/index.html` |

All three also create `v.mod`, `.editorconfig`, `.gitattributes` and
`.gitignore`.

## v.mod

```
Module {
	name: 'myapp'
	description: ''
	version: '0.0.0'
	license: 'MIT'
	dependencies: []
}
```

Creating one starts a prompt for the description, version and license. **The
prompt only runs when stdin is a terminal.** Piped or redirected — in CI, or
from a script — the defaults are used and `<name>` is required. So a
non-interactive run is:

```bash
v new myapp < /dev/null
```

If git is installed and the directory is not already a git project, `git init`
runs as part of setup.

## After it exists

```bash
cd myapp
v run .          # build and run
v -check .       # type-check only
v test .         # run the tests
```

The module name is `main` for the executable and web templates, and the project
name for the library template, so it matches the directory the file sits in.

## Resource Routing

- `references/TEMPLATES.md` - Read when choosing between `--bin`, `--lib` and
  `--web`, or when a template's contents need checking against the current
  compiler.

## Quick Reference

```bash
v new <name>          # executable project in <name>/
v new <name> --lib    # library project
v new <name> --web    # veb project
v init                # project in the current directory
v init --lib          # library in the current directory
```