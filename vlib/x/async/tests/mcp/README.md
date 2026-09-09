# mcp integration tests

These tests keep MCP traffic in memory. They do not start stdio, HTTP, a child
process, or any external MCP service.

The coverage target is the `x.async` boundary around MCP-shaped work:

- build an MCP request;
- decode and validate it inside a `Task`;
- produce an MCP response;
- decode the result after `Task.wait()`.
