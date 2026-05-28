# mcp integration example

This example uses MCP JSON-RPC values in memory and runs the dispatch step in an
`x.async.Task`. It does not start stdio, HTTP, a process transport, or any
external service.

The goal is to show the integration boundary:

- build an MCP request value;
- decode and validate it inside a task;
- return an MCP response value;
- consume the result with `Task.wait()`.

Run from the repository root:

```sh
./v run vlib/x/async/examples/mcp/tool_dispatch.v
```
