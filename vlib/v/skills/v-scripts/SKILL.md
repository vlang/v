---
name: v-scripts
description: Writing V as a scripting language - `.vsh` script mode, its implicit `import os` and unqualified `os` calls, running a script directly, the executable-cache contract that governs rebuilds, compiling a script without running it, and shebang scripts with no file extension. Use when a task is a build, release, maintenance or one-off automation job that would otherwise be bash, when deciding between a script and a small program, or when a `.vsh` file behaves differently from a `.v` file. Does not cover the language rules (see v-lang), the build and test loop (see v-workflow), writing test cases (see v-testing), or the wider command surface (see v-tools).
license: MIT
---

# V scripts

A file ending in `.vsh` is parsed in **script mode**. It is still V — the same
types, the same modules, the same compiler — but two rules make it read like a
shell script, and both are worth knowing before writing one, because they are
why a script can fail in a way a program would not.

## What script mode changes

**`os` is imported for you.** A `.vsh` file gets an implicit `import os` when it
does not already import `os` itself. If it does import `os` explicitly, that
import wins and keeps its own alias and selected symbols; the implicit one is
never added on top.

**`os` functions are called unqualified.** In script mode an unqualified
`read_file` resolves to `os.read_file`, so the usual `os.` prefix is optional:

```v ignore
println('hello')
content := read_file('data.txt') or { panic(err) }
```

That resolution reaches public `os` functions and constants (for example
`args`). A local or parameter of the same name shadows it, so naming a variable
`args` or `exists` changes what the bare name means — and that shadowing is
scoped to the script, not to a `.v` file compiled beside it.

Neither rule applies to a plain `.v` file. If a name resolves in a `.vsh` but not
in a `.v` file, this is why.

## Running and building scripts

Running the file runs the script; there is no separate interpreter to install.

```sh
v script.vsh                 # build if needed, then run
v run script.vsh             # rebuild and run, keep no executable
v build script.vsh           # compile to an executable, do not run it
v -skip-running script.vsh   # the same thing
```

On Unix-like systems a script that starts with this shebang runs by its path,
once it is executable (`chmod +x script.vsh`, then `./script.vsh`):

```sh ignore
#!/usr/bin/env -S v
```

For `.vsh` scripts on systems whose `/usr/bin/env` has no `-S` (BusyBox,
OpenBSD), put `cmd/tools/vrun` on the PATH and start the script with
`#!/usr/bin/env vrun` instead; it runs the script with `v run`.

`v script.vsh` follows **V's executable-cache contract**: it builds the script
as `script` (the name without `.vsh`) next to the source and reuses that binary
on the next run, which is what makes repeated invocation cheap. It rebuilds when
the script is newer than the binary, or when an imported module changed. The
`#!/usr/bin/env -S v` shebang goes through the same cache.

`v run script.vsh` skips the cache: it rebuilds on every run and leaves no
executable behind, and so do the `#!/usr/bin/env -S v run` and `vrun` shebangs.
The cached binary has to be deleted by hand: `v clean` handles `.v` files and
directories, not `.vsh` scripts.

## Scripts with no extension

A file with a fully custom name and a shebang runs as a script when it starts
with:

```sh ignore
#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp
```

`tmp` is the prefix for the built executable, which is kept as
`tmp.<filename>` beside the script. **Caution:** that name is overwritten if it
already exists. To rebuild every time instead of caching, use
`#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp run`.

This runs in `crun` mode. V's own documentation recommends it for scripts that go
on the PATH and advises against it for build or deploy scripts.

## Choosing between a script and a program

Reach for a script when the job is sequencing external tools, moving files,
watching a directory, or wrapping a build step. Reach for a program when there is
domain logic to test, when it needs a data structure, or when it will be imported.

The test suite exists to keep that boundary honest: if a `.vsh` has grown logic
worth asserting on, move it into a `_test.v` alongside a real program. Script
mode does not make a script untestable — it compiles like anything else.

## Worked examples in the tree

These scripts rely on script mode — no `import os`, unqualified `os` calls,
statements at the top level — and are worth reading before writing one:

- `examples/v_script.vsh` — creates, lists and removes files with `mkdir`, `ls`
  and `rm`
- `.github/problem-matchers/register_all.vsh` — reads `getenv`, then walks a
  folder with `walk_ext`

A larger `.vsh` can also be written like a program, with `import os` and a
`fn main`. The scripts bundled with the skills are written that way, for example
`vlib/v/skills/v-workflow/scripts/check.vsh`; their `#!/usr/bin/env -S v run`
shebang leaves no binary in the skill folder.

## Related Skills

- **The commands around a script**: see [v-tools](../v-tools/SKILL.md) for
  `v time`, `v retry`, `v repeat` and `v check-md`.
- **The build and test loop**: see [v-workflow](../v-workflow/SKILL.md).
- **The language rules**: see [v-lang](../v-lang/SKILL.md); a script is still V.