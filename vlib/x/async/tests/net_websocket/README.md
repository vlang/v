# net.websocket integration tests

These tests are intentionally in-memory and synthetic. They exercise
`websocket.Message` processing and callback-shaped control flow with `x.async`,
but they do not start a websocket server or claim end-to-end websocket server
coverage.

That limitation is important: in this V snapshot, `websocket.Server.close()` is
not a complete, stable shutdown primitive for a fragile validation test. The
tests therefore avoid binding ports and focus on the part `x.async` can safely
compose: message work, cancellation, and error propagation around callbacks.
