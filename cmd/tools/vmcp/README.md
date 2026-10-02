# V MCP tool server

`v mcp serve` exposes compiler tools over MCP stdio. `--read-only` omits the three
editing tools; `v mcp tools` prints the available catalog.

Tool argument schemas are JSON objects. Their descriptions preserve quotes,
backslashes and line breaks using JSON string escaping, so `tools/list` can be
decoded as a complete JSON response in both writable and read-only modes.

Paths stay inside the selected workspace even when an editing request creates a
new file. Existing symlink parents are resolved before the boundary is checked;
dangling symlinks are refused.
