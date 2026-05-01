# veb integration example

This example is a compile-time, in-memory `veb.Context` lifecycle sample. It
does not start a `veb` server and does not bind a port.

The example is intentionally small because robust server lifecycle validation
belongs to `veb` itself. Here, `x.async` only coordinates a synthetic handler
step and returns the response body through `Task.wait()`.

Run from the repository root:

```sh
./v run vlib/x/async/examples/veb/app_lifecycle.v
```
