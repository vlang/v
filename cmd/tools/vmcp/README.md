# V MCP tool server

`v mcp serve` exposes compiler tools over MCP stdio. `--read-only` omits the three
editing tools; `v mcp tools` prints the available catalog.

Tool argument schemas are JSON objects. Their descriptions preserve quotes,
backslashes and line breaks using JSON string escaping, so `tools/list` can be
decoded as a complete JSON response in both writable and read-only modes.

Paths stay inside the selected workspace even when an editing request creates a
new file. Existing symlink parents are resolved before the boundary is checked;
dangling symlinks are refused.

AST renderings over 400,000 bytes return `ast: null`, the original byte count,
`truncated: true`, the limit and reduction hints. Incomplete JSON trees are never
returned; smaller trees keep their normal AST object.

Compiler flags for `v_run`, `v_check` and `v_test_run` are placed before the
command and target. Program arguments keep their original boundaries, including
spaces, empty strings and shell punctuation; they are passed directly to the child.
Compiler children use the selected workspace as their working directory, so
relative paths in compiler flags and program file accesses are resolved there.
Their stdin is the null device: interactive reads receive EOF. Launch failures
return an error without terminating the server, and inaccessible workspaces are
refused before a POSIX child is started.

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
v mcp install                  # print the entry for every client already present
v mcp install opencode          # write it into that client's configuration
v mcp install --project         # the project-level file rather than the user one
v mcp install opencode --print  # print it, write nothing
v mcp uninstall opencode        # remove it again
v mcp uninstall --all           # remove it from every client that has it
```

Naming no client writes nothing, which is what makes `--print` the default
shape rather than a flag to remember.

### The clients

| Client | User file | Project file | Key |
| --- | --- | --- | --- |
| opencode | `~/.config/opencode/opencode.json` | `.opencode/opencode.json` | `mcp` |
| Claude Code | `~/.claude.json` | `.mcp.json` | `mcpServers` |
| Cursor | `~/.cursor/mcp.json` | `.cursor/mcp.json` | `mcpServers` |
| VS Code | `<config>/Code/User/mcp.json` | `.vscode/mcp.json` | `servers` |
| Zed | `<config>/Zed/settings.json` | none documented | `context_servers` |
| Gemini CLI | `~/.gemini/settings.json` | `.gemini/settings.json` | `mcpServers` |

`<config>` is the platform config directory: `%APPDATA%` on Windows, and
`~/.config` elsewhere. opencode is the odd one out and reads `~/.config` on
every platform.

opencode takes the executable and its arguments as one `command` array; the
rest take a `command` string plus `args`.

Every path here was read out of that client's own documentation or its live
configuration file. Zed documents no project-level file, so `v mcp install zed
--project` says so rather than inventing a path, and a missing Zed settings file
is reported rather than created, because its Windows location is not confirmed.
VS Code additionally reads the portable `.mcp.json` at a project root, which is
the same file Claude Code uses.

### What it will not do

The config file is edited textually, beside the servers the client already has.
A `json.decode`/`json.encode` round trip would reorder every key, because V maps
are unordered, and would quietly drop anything the decoder does not model. So:

- A file that is not plain JSON — comments, trailing commas — is **reported, not
  rewritten**. These files are meant to be edited by hand, and losing a comment
  to gain an entry is a bad trade.
- A file with no top-level key for the client is left alone rather than
  guessed at.
- An entry that is already there is not added twice.
- Zed's user-level file is never created from nothing, only added to if it
  exists.

Nothing else on your machine is touched: the entry names the compiler that is
running the tool, so it keeps working after `PATH` changes, and no token or
credential is ever written.
