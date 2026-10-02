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
