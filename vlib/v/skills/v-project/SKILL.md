---
name: v-project
description: Create V projects with v new or v init and choose a binary, library or web template.
license: MIT
---

# Creating a V project

Use this when starting a project, scaffolding a module, or choosing a template.
For building and testing, see `v-workflow`; for language rules, see `v-lang`; for
other commands, see `v-tools`.

Two commands, one decision: `v new` makes a directory, `v init` works in the one
you are already in.

## The decision

- `v new <name>` — creates `<name>/` and puts the project in it.
- `v init` — puts the project in the current directory. Use this when the
  directory already exists.

Both take the same three template flags, and both create a `v.mod`.

## Put template flags before the project name

Template flags belong to the `new` or `init` subcommand. For `new`, put them
after `new` and before the project name:

```bash
v new myapp           # executable, the default
v new --lib mylib     # library
v new --web myapp     # veb web app
```

`v new mylib --lib` fails with *"too many arguments"*: flag parsing stops at the
project name, so the later flag is read as another argument.

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
prompt only runs when stdin is a terminal.** With piped or redirected stdin,
the defaults are used. A non-interactive
`v new` requires the project name; `v init` derives it from the current directory.
For example:

```bash
v new myapp < /dev/null
```

If git is installed and the directory is not already a git project, `git init`
runs as part of setup.

## After it exists

```bash
cd myapp
v run .          # build and run an executable or web project
v -check .       # type-check only
v test .         # run the tests
```

The module name is `main` for executable and web templates. For a library, the
module name is the project name.

## Resource Routing

- `references/TEMPLATES.md` - Read when choosing between `--bin`, `--lib` and
  `--web`, or when a template's contents need checking against the current
  compiler.

## Quick Reference

```bash
v new <name>          # executable project in <name>/
v new --lib <name>    # library project
v new --web <name>    # veb project
v init                # project in the current directory
v init --lib          # library in the current directory
```
