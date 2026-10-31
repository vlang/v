# Optional Linux WebSocket reactor

`new_reactor()` and `Reactor.run()` serve already upgraded plaintext TCP sockets
on one explicit worker thread. Multiple reactors provide a fixed worker pool.
This API requires Linux epoll and eventfd; it does not change `Client` or `Server`.
HTTP parsing, origin/authentication checks, TLS termination, extension negotiation,
and automatic application heartbeats remain the caller's responsibility.

## Ownership and callbacks

Call `run()` exactly once, including after a pre-start `stop()`, so its resources
are released. Stop the reactor and join that worker before discarding it or the
callback `user` context. A second `run()` fails. Callbacks execute serially on the
worker and must not block. Close callbacks are deferred to avoid recursive calls
inside message or close handlers. Clone message payloads to retain them.

`attach(mut conn, response)` reserves a connection slot before returning success.
Pending attachments count against the connection limit. On failure, ownership
stays with the caller and no callback runs. On success, ownership transfers;
never access that TCP connection again. The supplied HTTP 101 response is queued
before all WebSocket frames. Every successful attach receives exactly one
`on_close`, including attachment failures and shutdown before `on_open`.

`ReactorClient` is a stable handle. `write_string`, `write`, `close`, and
`set_read_timeout` are safe from other threads. Posting from other threads copies
the payload. Success accepts a command, not a delivery receipt; a later failure
is reported by `on_close`. Concurrent producers retain their individual order;
there is no application-level total ordering across different producers.

`write` accepts final text, binary, ping, and pong frames. Text must be UTF-8 and
controls fit in 125 bytes. Receiving ping automatically queues the matching pong.
Fragmentation, masking, lengths, UTF-8, and close payloads use `ServerFrameDecoder`.
Compression is not negotiated or accepted. IDs are monotonic within each reactor
and poller events use these IDs instead of reusable file descriptors.

## Limits and fairness

Defaults are 16 KiB per incoming message, 64 pending output frames, 2 MiB of
pending output bytes, 10,000 accepted/pending connections, 8,192 mailbox commands,
and 16 MiB of mailbox payloads. The close frame has one reserved slot and at most
127 additional bytes so full data queues can still initiate a close handshake.
Sent prefixes are compacted and completed frame slots reclaimed. Allocation
capacity may exceed logical buffer length because of array growth.

An overflowing output queue or mailbox disconnects the affected peer with a
terminal notification. Invalid method arguments fail synchronously. New sends
are rejected once close or reactor shutdown has been requested. Each read and
write turn transfers at most 64 KiB; decoding defaults to 256 frames per turn.
Buffered frames are rescheduled without requiring another TCP arrival.

Read and write inactivity limits default to five seconds. Write progress resets
the write timer. Normal closing waits for the peer close reply for at most one
second by default, then reports an abnormal close (1006). Protocol failures may
close immediately after sending an error close frame. Deadline sweeps run every
50 ms; callbacks must remain short for those deadlines to be meaningful.

`stop()` rejects new work and begins a 1001 close handshake for existing sockets.
Pending attachments receive a terminal callback without opening. Nonresponsive
peers are bounded by `close_timeout`. After the worker returns, descriptors are
closed, all accepted connections are accounted for, and handles reject new work.

## Validation

```sh
./v -cc gcc -gc boehm vlib/net/websocket/frame_decoder_test.v
./v -cc gcc -gc boehm vlib/net/websocket/reactor_linux_test.c.v
```
