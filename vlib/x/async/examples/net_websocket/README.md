# net.websocket integration example

This example stays in memory around `websocket.Message` processing. It does not
open a websocket server, does not connect a client, and must not be read as an
end-to-end websocket server validation.

That boundary is deliberate. In this V snapshot, `websocket.Server.close()` is
not a complete, stable server shutdown mechanism suitable for fragile
validation. The public example therefore demonstrates the safe part that
`x.async` can own here: coordinating message/callback-style work and propagating
errors without inventing a websocket runtime.

Run from the repository root:

```sh
./v run vlib/x/async/examples/net_websocket/message_pipeline.v
```

No external service, fixed port, or websocket server lifecycle is used.
