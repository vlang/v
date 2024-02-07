# poly1305

Poly1305 is a one-time authenticator originally designed by D. J. Bernstein.
Poly1305 takes a 32-byte one-time key and a message and produces a
16-byte tag. It can be used to verify the data integrity and the authenticity of a message.

This module provides generic `poly1305` message authentication code (MAC) module in pure V.

As a note,  <b>a key must only be used for a single message</b>. Authenticating two different
messages with the same key allows an attacker to forge authenticators for other
messages with the same key.
