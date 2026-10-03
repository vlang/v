# The tools

> The working order is in the parent skill. This reference covers the arguments of
> each tool, and which question each one is the right answer to.

Every tool answers in JSON. Paths are relative to the workspace root unless the
tool says otherwise.

## Project

### `v_project_info`

Start here. Names the module, the compiler in use, the root every other path is
relative to, whether the tree is the V checkout itself, and whether skills are
installed.

```json
{"include": "installed_modules,skill_status"}
```

Both `include` values are optional and off by default, because they cost a
directory scan.

`compiler_version` and `v_version` are objects, not strings. They are
`{"value": "..."}` when the compiler answered, and `{"error": "..."}` when it could
not be started — so a sandbox that cannot spawn a process is never read as a
version string.

### `v_modules`

What the project can import: the declared `v.mod` dependencies, the modules already
installed for the current user, and the subdirectories that look like local
modules.

```json
{"filter": "http"}
```

Omit `filter` for everything.

### `v_files`

The V sources below a directory, with size and line count. Use it to find a file
before reading one.

```json
{"path": "src", "include_tests": false, "limit": 200}
```

A `path` that is not a directory is reported as an error rather than an empty list.

## Code

### `v_symbols`

What a file declares: functions, methods, structs and their fields, enums,
interfaces, sum types, constants and globals, each with line, column and doc
comment. The cheapest way to learn what a file is for.

```json
{"path": "src/parser.v", "kind": "fn", "include_nested": true}
```

`kind` filters (`fn`, `method`, `struct`, `field`, `enum`, `enum_value`, `const`,
`var`, `import`, `module`). `include_nested: false` drops members.

### `v_symbol_at`

What is written at a position, for example the cursor. `line` and `column` are
1-based and both required.

```json
{"path": "src/main.v", "line": 42, "column": 9}
```

With a `name`, it reports the occurrence and, when that occurrence is a
declaration, the declaration itself. Without one, it lists every declaration
covering the position, which is how you find out whether a cursor is on a
declaration or a call.

### `v_references`

Every mention of a name in a file, AST aware, so a comment or a string containing
the name is not reported. Each hit says whether it is the declaration.

```json
{"path": "src/main.v", "name": "parse_config"}
```

Reach for this before a rename: it tells you what the rename will cost.

### `v_ast`

The AST of one file, in exactly the format `v ast -p` prints.

```json
{"path": "src/main.v", "terse": true, "skip_defaults": true, "hide": ["pos"]}
```

`terse` keeps only node names and structure; `skip_defaults` drops zero-valued
properties. Both keep the answer small enough to read in full. A large file is
truncated, and the response says so.

### `v_stdlib_doc`

The documentation of a standard library module or one of its symbols.

```json
{"symbol": "strings"}
{"symbol": "strings.Builder"}
```

A bare module lists its documented symbols. A dotted name returns one member with
its signature, source file and line. An undocumented member reports `found: false`
with a hint rather than an empty object.

## Checking

### `v_check`

Type-check and return the diagnostics as records with file, line, column, kind and
message. This is the authoritative answer to "does it compile"; it compiles
nothing and writes nothing.

```json
{"path": "src", "flags": ["-d", "my_feature"]}
```

The response carries `started`. When it is `false` the compiler could not be
launched and the response has an `error` instead of a result — a sandbox failure
that would otherwise read as a failed check.

### `v_test_run`

Run the tests of a file or directory and report what passed, what failed and what
the failures said.

```json
{"path": "src", "only": "test_parse*", "silent": true}
```

`only` is the same glob `VTEST_ONLY_FN` takes. `env` passes extra variables.

### `v_doctor`

The state of the V installation: version, third-party directories, module and
cache locations. Run it before diagnosing a build failure, so the answer names the
actual cause instead of guessing.

### `v_veb_routes`

The HTTP methods, paths and handlers a veb application registers. The fastest way
to understand a web app you have not opened.

```json
{"path": "src/main.v"}
```

## Running

### `v_run`

Run a V program and return its exit code and output.

```json
{"target": "src/main.v", "args": ["--verbose"]}
```

### `v_eval`

Evaluate a short V snippet in process and return its captured output. Good for
checking an expression or a stdlib call without writing a file. It is the V
interpreter, so only the subset the interpreter supports is available; for anything
substantial, write a file and use `v_run`.

```json
{"code": "println(6 * 7)"}
```

## Environment

### `v_skills`

The agent skills bundled with this compiler and which are installed for this
project or for the current user, flagging any whose installed copy has fallen
behind the bundle. To add one: `v skills add <name>`.

## Resources

The server also publishes two, readable without a tool call:

| URI | What |
| --- | --- |
| `v://version` | the compiler version |
| `v://help/<topic>` | a `v help` topic |

And it publishes its model instructions at initialization, which describe the
working order. The same text is what `v mcp serve --instructions` prints.

## A note on `--read-only`

With `--read-only`, the three writing tools are not registered at all. A session
started that way has no way to edit through the server, which is stronger than a
flag being checked at call time.