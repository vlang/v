# poly1305

Poly1305 is a one-time authenticator originally designed by D. J. Bernstein.
Poly1305 takes a 32-byte one-time key and a message and produces a
16-byte tag. It can be used to verify the data integrity and the authenticity of a message.

This module provides generic `poly1305` message authentication code (MAC) module in pure V.

As a note, **a key must only be used for a single message**. Authenticating two different
messages with the same key allows an attacker to forge authenticators for other
messages with the same key.

> [!Warning]
> This is an experimental module, which is subject to change, please use it carefully and thoroughly

## Examples

```v
module main

import encoding.hex
import x.crypto.poly1305

fn main() {
	// this examples mostly based on rfc
	// provide yours secure key
	yourkey := '0000000000000000000000000000000036e5f6b5c5e06070f0efca96227a863e'
	key := hex.decode(yourkey)!

	// messages to be authenticated
	msg := 'Sample messages'.bytes()

	mut out := []u8{len: 16}
	// lets create tag (mac) stored into out
	poly1305.create_tag(mut out, msg, key)!
	status := poly1305.verify_tag(out, msg, key)
	assert status == true
}
```
