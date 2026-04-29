# x.async integration tests

This folder contains module-oriented integration tests for `x.async`.

Tests are validation-first programs. Unlike the examples, they are written to
assert edge cases, error propagation, cancellation behavior, backpressure, and
module integration boundaries. They should fail when a regression is introduced.

The tests are deliberately local and synthetic. They verify that `x.async`
composes cleanly with selected V modules without modifying those modules and
without depending on public network services, fixed ports, filesystem paths, or
fragile server shutdown behavior.

Current subfolders:

- `net_http/`: in-memory `net.http` request/response work through `Pool`.
- `net_websocket/`: in-memory `websocket.Message` processing and callback-style
  error propagation through `Task` and `Group`.
- `mcp/`: in-memory MCP JSON-RPC request/response dispatch through `Task`.
- `veb/`: in-memory `veb.Context` response lifecycle through `with_timeout()`.

Run through the guarded validation path:

```sh
sh vlib/x/async/tools/validate.sh
```

The validation script uses isolated `VTMP` and `VCACHE` and runs serially.
