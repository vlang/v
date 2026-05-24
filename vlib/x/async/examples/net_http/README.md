# net.http integration example

This example shows how `x.async` can coordinate local `net.http` request and
response handling without opening a network listener.

It is intentionally synthetic: requests are constructed in memory, processed by
a small handler, and drained through a `Pool`. This keeps the example stable
while still showing the lifecycle pattern used by HTTP-facing code:

- construct or receive a `http.Request`;
- submit bounded work to an `x.async.Pool`;
- publish a `http.Response` result;
- close the pool and report the first error if one occurred.

Run from the repository root:

```sh
./v run vlib/x/async/examples/net_http/request_batch.v
```

No external service, fixed port, file path, or real HTTP server is used.
