# Incremental server frame decoder

`ServerFrameDecoder.decode(mut input)` processes one masked client frame without
performing socket I/O. It leaves incomplete input unchanged. Retain that input,
append the next TCP bytes, and retry. On success, discard `consumed` bytes only
after using the returned payload. Continue until `kind` is `need_more`.

`message` contains a complete text or binary message. `fragment` consumes an
intermediate data fragment without delivering a message. `control` contains a
ping, pong, or validated close frame. Control frames may interrupt fragmented
messages. Replying to controls and performing the close handshake are the
transport's responsibility.

Payloads are borrowed from input or the decoder's fragment buffer. They are
valid until input changes or the next decode call. Clone payloads to retain them.
Each connection needs its own decoder; concurrent access is not supported.

The default message limit is 16 KiB; `max_message_bytes` must be positive.
Lengths are checked before allocation,
including the total length of fragmented messages. Text is validated as UTF-8;
invalid fragment prefixes are rejected immediately and valid split runes are
retained. Reserved bits/opcodes, invalid continuations, unmasked client frames,
noncanonical lengths, invalid close codes, and invalid close reasons fail with
the corresponding WebSocket close code. A failure is terminal.

The decoder has no network, scheduler, compression, or TLS dependency. It does
not replace the existing blocking `Client` parser.
