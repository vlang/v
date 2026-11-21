## Description

`net.sasl` holds building blocks for SASL (RFC 4422) mechanisms that do not need
a cryptographic exchange. For SCRAM, see `crypto.scram`.

It currently implements the PLAIN mechanism (RFC 4616): a single message from the
client, made of an optional authorization identity, an authentication identity and a
password, separated by NUL bytes. `encode_plain` builds that message, and `parse_plain`
splits and validates it on the server side.

Both functions work on the raw message. Protocols such as SMTP, IMAP and IRC carry it
base64-encoded; the transport does that encoding, not this module.

PLAIN sends the password in clear text. Only use it over a connection that is already
encrypted, such as TLS.

## Client

```v
import encoding.base64
import net.sasl

fn main() {
	message := sasl.encode_plain(authcid: 'tim', password: 'tanstaaftanstaaf')!
	println('AUTH PLAIN ${base64.encode_str(message)}')
}
```

`encode_plain` returns an error when `authcid` or `password` is empty, or when a field
contains a NUL byte or invalid UTF-8, so a bad credential is caught before anything is
sent. Leave `authzid` empty to act as `authcid`. The fields are sent unchanged: preparing
them is the server's job, so only prepare them on the client in the way agreed with the
server.

## Server

```v
import net.sasl

fn main() {
	m := sasl.parse_plain('\x00tim\x00tanstaaftanstaaf')!
	println(m.authcid) // tim
	println(m) // the password is redacted
}
```

`parse_plain` only checks the format. The server still has to:

* prepare `authcid` and the password the way it stored them (RFC 4616 recommends
  SASLprep), and fail if the preparation fails or leaves either of them empty:
  `parse_plain` only checks the fields as received;
* check the password against its own store, in constant time;
* decide whether `authcid` may act as `authzid` when the client sent one.

RFC 4616 requires servers to accept fields of up to 255 bytes each. `parse_plain`
imposes no limit; bound the size of the message in the transport.
