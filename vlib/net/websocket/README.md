# Websocket callbacks

Websocket event callbacks have result signatures, such as
`fn (mut client websocket.Client, message &websocket.Message) !`.
Callbacks that return `void` can also be registered: returning normally is treated as success.
The C backend adapts their return value so the listener receives a valid successful result.

For callbacks registered with `on_message_ref`, the last `voidptr` argument is the original
reference passed at registration. Cast it back to its concrete reference type in an `unsafe`
block, and keep the referenced object alive while the callback can run. Such a reference can
be forwarded as `mut unsafe { &Context(ctx) }` to a function taking `mut context &Context`.

A server's `on_connect` callback runs before its handshake response is written. Use
`on_attached` or `on_attached_ref` to push frames after the handshake response has been written
and the client's callbacks have been registered.

## Explicit write batching

`client.write_messages([]websocket.Message)` sends the supplied complete frames
in order without waiting to collect more traffic. It uses one socket write and
returns the wire byte count, including headers. Empty batches are a no-op.
Control frames must be at most 125 bytes; continuation frames are not accepted.
Close frames are not accepted; call `client.close(code, reason)` for the closing handshake.
The entire batch is validated before any frames are sent.
Socket writes, including pongs from the reader, are serialized per connection.
On a write error, close the connection: a prefix may already have been sent.

## Incremental server frame decoding

`ServerFrameDecoder.decode(mut input)` processes masked client frames from a
caller-owned buffer without performing socket I/O. It supports incomplete input,
fragmented messages, interleaved controls, message limits, and protocol validation.
See [the decoder documentation](FRAME_DECODER.md) for buffer ownership and usage.
