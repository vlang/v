## Description

`crypto.scram` implements the Salted Challenge Response Authentication
Mechanism (SCRAM) family of SASL mechanisms: `SCRAM-SHA-1` (RFC 5802),
`SCRAM-SHA-256` (RFC 7677) and `SCRAM-SHA-512`, with and without channel
binding.

SCRAM is a password authentication protocol that never puts the password
on the wire. The client proves it knows the password, the server proves
it too, and neither side can replay a recorded exchange:

* the password is stretched with PBKDF2 under a per-user salt, so the
  server stores two derived keys instead of anything reversible;
* what the client sends is a *proof* computed over both nonces and both
  messages, so it is worthless outside the exchange that produced it;
* the server signs the same transcript back, so a client that reaches
  the end of the exchange knows it is talking to a server that holds the
  real credentials, not to something that merely accepted its proof.

It is what PostgreSQL 10+, MongoDB 3.0+, Kafka, LDAP, XMPP and the SASL
profiles of IMAP and SMTP authenticate with. If you are writing a driver
for any of them in V, this is the piece that used to be missing.

The module has no dependencies outside `vlib` and no C dependency: it is
built on `crypto.hmac`, `crypto.sha1`, `crypto.sha256`, `crypto.sha512`,
`crypto.rand`, `crypto.subtle` and `encoding.base64`.

## Usage

The exchange is four messages. The client produces the first and third,
the server the second and fourth. That maps to three calls on `Client`
and two on `Server`, each one taking the message the peer just sent and
returning the message to send back.

### Authenticating against a server

This is the case you want most of the time. Everything the transport has
to do is carry four opaque ASCII strings.

```v ignore
import crypto.scram

mut client := scram.new_client(username: 'user', password: 'pencil')!

// 1. announce the mechanism and send the first message
send(client.mechanism_name(), client.first()!)
// 2. answer the server challenge
final := client.final(receive())!
send_payload(final)
// 3. check that the server proved itself too — never skip this step
client.verify(receive())!
```

Do not treat the connection as authenticated before `verify` returns. A
server that answers the first three messages and fails the fourth does
not hold your credentials.

### Storing credentials

A server never stores a password. `new_credentials` derives the record it
does store, with a fresh random salt:

```v
import crypto.scram

fn main() {
	credentials := scram.new_credentials(.sha256, 'pencil')!
	// Persist all four fields; none of them lets you recover the password.
	println(credentials.mechanism.name())
	println(credentials.salt.len)
	println(credentials.iterations)
	println(credentials.stored_key.len)
}
```

Use `derive_credentials` instead when the salt and iteration count are
imposed, for instance to reproduce a record another implementation wrote.

`encode` and `parse_credentials` turn a record into one line and back, in
the format of RFC 5803 laid out in the `authPassword` syntax of RFC 3112:

```v
import crypto.scram

fn main() {
	credentials := scram.derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096)!
	line := credentials.encode()
	// SCRAM-SHA-256$4096:c2FsdHNhbHRzYWx0c2FsdA==$Y7KMtn...:c1MMj1...
	restored := scram.parse_credentials(line)!
	assert restored.stored_key == credentials.stored_key
}
```

That is the same layout PostgreSQL stores in `pg_authid.rolpassword`, so a
record written here can be read by a PostgreSQL server or an LDAP
directory, and the other way round. Treat the line as a secret: the server
key in it is enough to impersonate the server to that user.

Printing is safe by default. `Client`, `Server` and `Credentials` define
their own `str()`, so a `println(client)` while debugging shows the state
and the user name but never the password or the keys — V would otherwise
format every field, including the secrets.

### Authenticating a client

`Server` reads credentials through a callback, so it does not care where
they are stored:

```v
import crypto.scram

fn main() {
	credentials := scram.new_credentials(.sha256, 'pencil')!

	mut server := scram.new_server(
		lookup: fn [credentials] (username string) !scram.Credentials {
			// Look the user up in your database here.
			return credentials
		}
	)!
	mut client := scram.new_client(username: 'user', password: 'pencil')!

	server_first := server.first(client.first()!)!
	server_final := server.final(client.final(server_first)!)!
	client.verify(server_final)!

	println('${server.username()} authenticated: ${client.done() && server.done()}')
}
```

When `final` returns an `AuthenticationFailed`, answer the client with
`scram.server_error_message('invalid-proof')` rather than closing the
connection, so it can tell a refusal from a network failure.

## Errors

Four typed errors let a caller react without parsing strings:

| Error | Meaning |
| :--- | :--- |
| `MalformedMessage` | the peer did not send a valid SCRAM message |
| `AuthenticationFailed` | the peer failed to prove what it claimed |
| `UnsupportedMechanism` | `mechanism_from_name` did not recognise a name |
| `ServerError` | the server refused with an `e=` code |

Calling the steps out of order, or configuring a client without a user
name, returns a plain `error()` instead: those are bugs in the calling
code, not protocol outcomes.

## Channel binding

Channel binding ties the exchange to the TLS connection carrying it, so
that a proxy which terminates TLS cannot relay a valid exchange. It is
what the `-PLUS` mechanism names mean.

This module does not reach into the TLS layer: pass the binding data in
and it will use it.

```v
import crypto.scram

fn main() {
	binding := scram.ChannelBinding{
		mode: .required
		name: 'tls-server-end-point' // RFC 5929; or 'tls-exporter', RFC 9266
		data: certificate_hash()
	}
	mut client := scram.new_client(
		username:        'user'
		password:        'pencil'
		channel_binding: binding
	)!
	println(client.mechanism_name()) // SCRAM-SHA-256-PLUS
}

fn certificate_hash() []u8 {
	return []u8{len: 32}
}
```

`data` is mandatory when `mode` is `.required`: a binding with a name but
no data is refused at construction, because it would otherwise announce a
`-PLUS` mechanism and complete an exchange that binds nothing.

If the server advertised no `-PLUS` mechanism but your client supports
channel binding, set `mode: .unsupported_by_server`. That sends the GS2
`y` flag, which lets a server that *does* offer `-PLUS` detect that its
advertised list was stripped in transit. Leaving the default
`.not_supported` in that situation silently gives up the protection.

## Security notes

**Normalisation.** RFC 5802 passes the password through SASLprep
(RFC 4013) before hashing. V has no stringprep implementation, so this
module hashes the UTF-8 bytes it is given. ASCII passwords are
unaffected. For a non-ASCII password, normalise it before calling, or
two clients spelling the same password differently will disagree.

**Iteration count.** A client refuses a server asking for fewer than
`default_min_iterations` (4096, the floor in RFC 7677 §4), because a low
count makes an offline attack on a recorded exchange cheap. Lower it
with `ClientConfig.min_iterations` only for a legacy server that leaves
no choice. `new_credentials` writes `default_iterations` (32768).

A client also refuses a count above `default_max_iterations` (2^20). The
count is chosen by the server and consumed before the server has been
authenticated, so without a ceiling a hostile endpoint turns a short
message into minutes of client CPU. Raise it with
`ClientConfig.max_iterations` if you really talk to a server that asks
for more.

**User enumeration.** Returning an error from the `lookup` callback tells
the caller — and through it, an attacker — that a user name does not
exist. RFC 5802 §7 suggests answering unknown users with credentials
derived from a server-side secret, so they are indistinguishable from a
wrong password. That policy belongs to the application, which is why the
module leaves it in the callback.

**Timing.** Proofs and signatures are compared with
`crypto.subtle.constant_time_compare`.

**What SCRAM does not do.** It authenticates, it does not authorize:
`Server.authzid()` is a request from the client, not a granted right.
And without channel binding, SCRAM over an unauthenticated channel is
still vulnerable to a relay; use it over TLS.

## Conformance

`conformance_test.v` drives both halves of the exchange against eight
vectors and checks all four messages byte for byte, plus the salted
password, stored key and server key behind them.

The first two vectors are the normative examples of RFC 5802 §5 and
RFC 7677 §3, transcribed from the RFC text. The other six cover what the
RFCs leave without an example: SHA-512, a user name needing `saslname`
escaping, an authorization identity, channel binding and a non-ASCII
password.

All eight were generated by an implementation written independently from
RFC 5802 §3, then replayed through `github.com/xdg-go/scram` v1.2.0 — the
library the MongoDB Go driver authenticates with — which agrees on every
message. `Hi()` is additionally checked against `crypto.pbkdf2`, which is
an unrelated implementation of the same primitive already in vlib.

## References

* RFC 5802 — SCRAM-SHA-1 and the SCRAM family
* RFC 7677 — SCRAM-SHA-256
* RFC 5801 — the GS2 header
* RFC 5929 — `tls-server-end-point` channel binding
* RFC 9266 — `tls-exporter` channel binding
* RFC 4013 — SASLprep