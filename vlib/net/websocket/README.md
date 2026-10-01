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
