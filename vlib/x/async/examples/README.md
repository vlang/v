# x.async examples

This folder contains short public examples for the current `x.async` API.
Each file focuses on one idea and uses only local, in-memory work.

Examples are documentation-first programs. They show how the public API is
expected to be used in small readable scenarios. They are executed by the
validation script to avoid bit rot, but the regression guarantees live in the
`tests/` folder.

## Rules

- No external service is required.
- No fixed local path is used.
- No network listener is opened.
- Examples avoid `panic()` as a control-flow pattern.
- Cancellation remains cooperative; examples show jobs checking `ctx.done()`
  when they may run for more than a tiny unit of work.

## Examples

- `basic_group.v`: first error propagation and cooperative sibling
  cancellation.
- `basic_task.v`: one value-returning task consumed with `wait()`.
- `worker_pool.v`: fixed concurrency and explicit `try_submit()` backpressure.
- `periodic.v`: a blocking `every()` loop stopped by context cancellation.
- `timeout.v`: one cooperative job bounded by `with_timeout()`.
- `net_http/`: synthetic `net.http` request/response processing with `Pool`.
- `net_websocket/`: in-memory `websocket.Message` processing. It is not an
  end-to-end websocket server example.
- `mcp/`: in-memory MCP request/response dispatch with `Task[T]`.
- `veb/`: synthetic `veb.Context` response lifecycle with `Task[T]`.

## Run

From the repository root:

```sh
./v run vlib/x/async/examples/basic_group.v
./v run vlib/x/async/examples/basic_task.v
./v run vlib/x/async/examples/worker_pool.v
./v run vlib/x/async/examples/periodic.v
./v run vlib/x/async/examples/timeout.v
./v run vlib/x/async/examples/net_http/request_batch.v
./v run vlib/x/async/examples/net_websocket/message_pipeline.v
./v run vlib/x/async/examples/mcp/tool_dispatch.v
./v run vlib/x/async/examples/veb/app_lifecycle.v
```

Use `tools/validate.sh` for the guarded module validation path. It isolates V
temporary/cache directories and keeps validation runners serialized.
