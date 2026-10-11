# ASCII fast path for reactor text validation

The incremental server decoder validates complete incoming text messages, and the
Linux reactor validates each outgoing text frame. The fast path recognizes ASCII
in bounded eight-byte words. Any non-ASCII
byte delegates the entire payload to `encoding.utf8.validate`, including its prefix.
Fragmented incoming text retains the incremental Unicode state machine.

The word loads use fixed-size `memcpy`, allowing unaligned input and avoiding
aliasing assumptions. They stop before the end of the payload; shorter tails use
byte reads. ASCII controls and zero bytes remain valid UTF-8, as before.

This changes neither accepted text nor error handling. Close reasons still use
the existing UTF-8 validator directly; the original `Client` and `Server` paths
are unchanged.

Run `frame_utf8_test.v` with assertions enabled, including under ASan/UBSan.
Use `-cflags -O2` to optimize native tests while retaining V assertions.
