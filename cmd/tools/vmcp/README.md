# V MCP tool server

`v mcp serve` exposes compiler tools over MCP stdio. `--read-only` omits the three
editing tools; `v mcp tools` prints the available catalog.

Tool argument schemas are JSON objects. Their descriptions preserve quotes,
backslashes and line breaks using JSON string escaping, so `tools/list` can be
decoded as a complete JSON response in both writable and read-only modes.
Tools omit the optional `title` that would repeat their stable `name`.
Tests limit raw catalogue strings to 12,000 bytes and the complete `tools/list`
stdio response, including its newline, to 13,000 bytes. These are byte budgets;
token counts depend on the client's model.

Paths stay inside the selected workspace even when an editing request creates a
new file. Existing symlink parents are resolved before the boundary is checked;
dangling symlinks are refused.

AST renderings over 400,000 bytes return `ast: null`, the original byte count,
`truncated: true`, the limit and reduction hints. Incomplete JSON trees are never
returned; smaller trees keep their normal AST object.

`v_stdlib_doc` module listings accept `limit` (default 100) and `offset` (default 0).
Nonpositive limits use the default; negative offsets start at zero. An offset past
the last symbol returns an empty page. `symbol_count` remains the total, while
`returned` and `truncated` describe the page. Queries for a specific member return
that member in full. `v_files` defaults to 500 entries and caps explicit limits at 2000.

Compiler flags for `v_run`, `v_check` and `v_test_run` are placed before the
command and target. Program arguments keep their original boundaries, including
spaces, empty strings and shell punctuation; they are passed directly to the child.
Compiler children use the selected workspace as their working directory, so
relative paths in compiler flags and program file accesses are resolved there.
Their stdin is the null device: interactive reads receive EOF. Launch failures
return an error without terminating the server, and inaccessible workspaces are
refused before a POSIX child is started.

`v_check`, `v_test_run` and `v_run` accept `max_diagnostics`, defaulting to 100.
Zero and negative values use that default. Responses keep the first diagnostics
and report `diagnostics_omitted` plus a hint when the array is shortened.
`error_count` and `warning_count` describe all parsed diagnostics, including the
omitted ones. `v_run` still returns its program output with the existing line limit.

`v_format` returns parser diagnostics for malformed source and leaves the file
untouched, including when `write: true` is requested.

`v_rename_symbol` includes named struct initializer and update keys, so renaming a
field also updates values initialized with that field name.

Every installed help text is listed as a readable `v://help/<topic>` resource.
The `v://help/{topic}` template describes those same registered topics.

## Registering with a coding agent

`v mcp serve` speaks MCP over stdio, which is the transport editors launch, so a
client has to be told about it before its tools appear. `v mcp install` does
that.

```sh
v mcp install                   # print the entry for every client already present
v mcp install opencode          # write it into that client's configuration
v mcp install --project         # the project-level file rather than the user one
v mcp install opencode --print  # print it, write nothing
v mcp uninstall opencode        # remove it again
v mcp uninstall --all           # remove it from every client that has it
```

Naming no client writes nothing, which is what makes `--print` the default
shape rather than a flag to remember.

The printed JSON is a complete object when the file does not exist, a top-level
member when the client's server key is absent, and a quoted server member when
the key is already present. Read failures are reported. Printing keeps project
scope restrictions and identifies files that the installer will not create.

### The clients

| Client | User file | Project file | Key |
| --- | --- | --- | --- |
| opencode | `~/.config/opencode/opencode.json` | `opencode.json` | `mcp` |
| Claude Code | `~/.claude.json` | `.mcp.json` | `mcpServers` |
| Cursor | `~/.cursor/mcp.json` | `.cursor/mcp.json` | `mcpServers` |
| VS Code | `<config>/Code/User/mcp.json` | `.vscode/mcp.json` | `servers` |
| Zed | `<zed>/settings.json` | not handled | `context_servers` |
| Gemini CLI | `~/.gemini/settings.json` | `.gemini/settings.json` | `mcpServers` |

`<config>` is the platform config directory: `%APPDATA%` on Windows,
`~/Library/Application Support` on macOS, and `$XDG_CONFIG_HOME` (default
`~/.config`) elsewhere. opencode is the odd one out and reads `~/.config` on
every platform.

`<zed>` is where Zed itself looks: `%APPDATA%\Zed` on Windows, `~/.config/zed`
on macOS, and on Linux and FreeBSD `$FLATPAK_XDG_CONFIG_HOME/zed` inside a
Flatpak, otherwise `$XDG_CONFIG_HOME/zed` (default `~/.config/zed`).

opencode's project file is `opencode.json` at the project root. When
`opencode.jsonc`, `.opencode/opencode.json` or `.opencode/opencode.jsonc` is
already there instead, that file is the one edited, in the order opencode's own
`opencode mcp add` picks.

opencode takes the executable and its arguments as one `command` array; the
rest take a `command` string plus `args`.

Each path follows where that client reads its configuration. Only Zed's
user-level settings file is handled, so `v mcp install zed --project` says so
rather than picking a path, and a missing Zed settings file is reported rather
than created: it holds every other Zed setting too, so it is only ever added to.
A missing file for any other client is created with the entry in it.

### What it will not do

The config file is edited textually, beside the servers the client already has.
A `json.decode`/`json.encode` round trip would reorder every key, because V maps
are unordered, and would quietly drop anything the decoder does not model. So:

- Line comments and complete block comments around server entries are preserved during
  textual installation and removal. Comment markers inside strings remain string values.
- Invalid JSON, trailing commas, and unterminated block comments are reported and left
  unchanged.
- A valid JSON value whose top level is not an object is left untouched. The
  error identifies the missing root object.
- A file with no top-level key for the client is not refused: the key goes in,
  holding the entry, as the first member of the root object. A key whose value
  is not an object is reported rather than guessed at.
- An entry that is already there stays unchanged, and installation succeeds.
  When its command can be read, the installer prints its executable and arguments.
  If the executable path differs from this compiler, it names this compiler and
  suggests uninstall/install commands to move the entry. These commands keep
  `--project` when the entry belongs to the project configuration and invoke
  this compiler by its full path, even when `v` on `PATH` names another compiler.
  Windows guidance uses PowerShell syntax.
- Zed's user-level file is never created from nothing, only added to if it
  exists.
- `v mcp uninstall` leaves a file with invalid JSON or trailing commas alone too, says the
  entry has to be removed by hand, and exits with status 1.
- A file is never left half-written: the new text goes to a temporary file
  beside it, which then replaces it in one step. A symlinked config stays a
  symlink, and keeps its permissions.

When a file cannot be parsed after removing comments, `v mcp install` leaves it unchanged
and prints the
client's top-level member for pasting by hand. If that key already exists, merge the
`vlang` server member into it instead of adding a second key. Other failures report
why the file could not be edited. Refused installations exit with status 1.

The registered command uses the executable named by `VEXE` when available. If that
path is missing or is not an executable file, it tries the compiler path recorded when the
tool was built. Directories and non-executable files are skipped. Paths are resolved
before registration, and the `.exe` form is preferred on Windows.

Nothing else on your machine is touched: the entry names the compiler that is
running the tool, so it keeps working after `PATH` changes, and no token or
credential is ever written.
