// Package scram implements the Salted Challenge Response Authentication
// Mechanism (SCRAM) family of SASL mechanisms:
//
// * `SCRAM-SHA-1`   — RFC 5802
// * `SCRAM-SHA-256` — RFC 7677
// * `SCRAM-SHA-512` — same construction, registered with IANA
//
// SCRAM lets a client prove that it knows a password without ever sending
// it, and lets the client verify in the same round trip that the server
// knows it too (mutual authentication). The server stores only two derived
// keys, so a stolen credential database does not directly hand out
// passwords, and every exchange is salted and nonce'd, so recordings of an
// exchange cannot be replayed.
//
// It is the authentication mechanism of PostgreSQL (10+), MongoDB (3.0+),
// Kafka (SASL/SCRAM), LDAP, XMPP and the SASL profiles of IMAP and SMTP.
//
// The exchange is four messages: this module models it as three calls on the
// client and two on the server, each one taking the message the peer just
// sent and returning the message to send back. Driving both halves in
// process, which is also what a test does, shows the whole protocol:
//
// Example:
// ```v
// creds := scram.new_credentials(.sha256, 'pencil')!
// mut server := scram.new_server(lookup: fn [creds] (username string) !scram.Credentials {
// 	return creds
// })!
// mut client := scram.new_client(username: 'user', password: 'pencil')!
//
// server_first := server.first(client.first()!)!
// server_final := server.final(client.final(server_first)!)!
// client.verify(server_final)!
//
// assert client.done() && server.done()
// ```
//
// Against a real server only the client half is used: send `client.first()`,
// feed the reply to `client.final()`, send that, and hand the last reply to
// `client.verify()`. The transport is not this module's concern — SCRAM
// messages are ASCII strings that a protocol such as PostgreSQL's or
// MongoDB's carries as SASL payloads.
//
// Both `Client` and `Server` are single-use: one value drives exactly one
// authentication exchange, and calling the steps out of order is an error
// rather than undefined behaviour.
module scram

import crypto.hmac
import crypto.rand
import crypto.sha1
import crypto.sha256
import crypto.sha512
import encoding.base64

// default_min_iterations is the smallest PBKDF2 iteration count a client
// accepts from a server unless `ClientConfig.min_iterations` says otherwise.
// RFC 7677 §4 requires clients to reject anything below 4096, because a low
// count makes an offline attack on an intercepted exchange cheap.
pub const default_min_iterations = 4096

// default_max_iterations is the largest PBKDF2 iteration count a client
// accepts from a server unless `ClientConfig.max_iterations` says otherwise.
// The count is a number a server picks, and deriving the salted password
// happens before anything about that server has been authenticated, so
// without a ceiling a hostile endpoint turns a few bytes of
// server-first-message into unbounded work on the client: at the 999999999
// the grammar allows, that is tens of minutes of CPU per connection. 2^20 is
// three orders of magnitude above what deployments use in practice.
pub const default_max_iterations = 1_048_576

// default_iterations is the iteration count used by `new_credentials` when
// the caller does not pick one. RFC 7677 §4 gives 4096 as the floor; this
// module defaults an order of magnitude above it, which stays well under a
// millisecond of CPU per login on current hardware.
pub const default_iterations = 32768

// default_salt_size is the number of random bytes `new_credentials` uses for
// a salt, matching the 16 octets recommended by RFC 5802 §5.1.
pub const default_salt_size = 16

// nonce_size is the number of random bytes behind a generated nonce. The
// base64 spelling that goes on the wire is longer, and comfortably above the
// entropy RFC 5802 §5.1 asks for.
const nonce_size = 24

// Mechanism selects the hash function of a SCRAM exchange, which is the only
// thing that varies between the members of the SCRAM family.
pub enum Mechanism {
	sha1
	sha256
	sha512
}

// name returns the IANA SASL mechanism name, such as `SCRAM-SHA-256`.
pub fn (m Mechanism) name() string {
	return match m {
		.sha1 { 'SCRAM-SHA-1' }
		.sha256 { 'SCRAM-SHA-256' }
		.sha512 { 'SCRAM-SHA-512' }
	}
}

// name_plus returns the channel binding spelling of the mechanism name, such
// as `SCRAM-SHA-256-PLUS`. Advertise this name when the exchange is bound to
// the underlying TLS channel, i.e. when `ChannelBinding.mode` is `.required`.
pub fn (m Mechanism) name_plus() string {
	return '${m.name()}-PLUS'
}

// size returns the digest length of the mechanism in bytes, which is also the
// length of every key and proof it produces.
pub fn (m Mechanism) size() int {
	return match m {
		.sha1 { sha1.size }
		.sha256 { sha256.size }
		.sha512 { sha512.size }
	}
}

// mechanism_from_name maps an IANA SASL mechanism name to a `Mechanism`.
// Both spellings are accepted, so `SCRAM-SHA-256` and `SCRAM-SHA-256-PLUS`
// both return `.sha256`; whether channel binding is in use is carried by
// `ChannelBinding`, not by the mechanism. Use it to pick a mechanism from the
// list a server advertises.
pub fn mechanism_from_name(name string) !Mechanism {
	match name {
		'SCRAM-SHA-1', 'SCRAM-SHA-1-PLUS' {
			return .sha1
		}
		'SCRAM-SHA-256', 'SCRAM-SHA-256-PLUS' {
			return .sha256
		}
		'SCRAM-SHA-512', 'SCRAM-SHA-512-PLUS' {
			return .sha512
		}
		else {
			return UnsupportedMechanism{
				name: name
			}
		}
	}
}

// block_size returns the internal block size of the hash, needed by HMAC.
fn (m Mechanism) block_size() int {
	return match m {
		.sha1 { sha1.block_size }
		.sha256 { sha256.block_size }
		.sha512 { sha512.block_size }
	}
}

// hash is the `H()` of RFC 5802 §2.2.
fn (m Mechanism) hash(data []u8) []u8 {
	return match m {
		.sha1 { sha1.sum(data) }
		.sha256 { sha256.sum256(data) }
		.sha512 { sha512.sum512(data) }
	}
}

// hmac is the `HMAC()` of RFC 5802 §2.2.
fn (m Mechanism) hmac(key []u8, data []u8) []u8 {
	return match m {
		.sha1 { hmac.new(key, data, sha1.sum, sha1.block_size) }
		.sha256 { hmac.new(key, data, sha256.sum256, sha256.block_size) }
		.sha512 { hmac.new(key, data, sha512.sum512, sha512.block_size) }
	}
}

// hi is the `Hi(str, salt, i)` of RFC 5802 §2.2: PBKDF2 (RFC 2898) with HMAC
// as the pseudorandom function and an output length equal to the digest size.
// Because exactly one block is requested, the block index is the constant
// `INT(1)` and no outer loop over blocks is needed.
fn (m Mechanism) hi(password []u8, salt []u8, iterations int) []u8 {
	mut block := []u8{cap: salt.len + 4}
	block << salt
	block << [u8(0), 0, 0, 1]
	mut u := m.hmac(password, block)
	mut result := u.clone()
	for _ in 1 .. iterations {
		u = m.hmac(password, u)
		for i in 0 .. result.len {
			result[i] ^= u[i]
		}
	}
	return result
}

// Credentials is what a SCRAM server stores for one user. It is derived from
// the password but does not allow recovering it, and `stored_key` alone is
// not enough to authenticate as the user — that is the whole point of the
// mechanism. `server_key`, on the other hand, does let its holder impersonate
// the *server*, so it deserves the same protection as any secret.
pub struct Credentials {
pub:
	// mechanism is the hash the keys below were derived with. Credentials are
	// not interchangeable between mechanisms.
	mechanism Mechanism
	// salt is the per-user random salt, sent to the client in cleartext.
	salt []u8
	// iterations is the PBKDF2 iteration count used to derive the keys.
	iterations int
	// stored_key is `H(HMAC(SaltedPassword, "Client Key"))`, used to check the
	// proof a client sends.
	stored_key []u8
	// server_key is `HMAC(SaltedPassword, "Server Key")`, used to sign the
	// server-final-message so the client can authenticate the server.
	server_key []u8
}

// str renders Credentials without their key material. `stored_key` and
// `server_key` are secrets — `server_key` in particular lets its holder
// impersonate the server — and V formats structs automatically, so the
// default rendering would leak them into any log that prints a record.
pub fn (c Credentials) str() string {
	return 'scram.Credentials{ mechanism: ${c.mechanism.name()}, salt: ${c.salt.len} bytes, iterations: ${c.iterations} }'
}

// derive_credentials computes the credentials a server stores for `password`,
// using a salt and iteration count the caller chooses. Use it when you need
// to reproduce an existing record; prefer `new_credentials` for a fresh user,
// as it picks a random salt for you.
//
// The password must already be normalised: see the SASLprep note in the
// module README.
pub fn derive_credentials(mechanism Mechanism, password string, salt []u8, iterations int) !Credentials {
	if salt.len == 0 {
		return error('scram: the salt must not be empty')
	}
	if iterations < 1 {
		return error('scram: the iteration count must be at least 1, got ${iterations}')
	}
	salted := mechanism.hi(password.bytes(), salt, iterations)
	client_key := mechanism.hmac(salted, 'Client Key'.bytes())
	return Credentials{
		mechanism:  mechanism
		salt:       salt.clone()
		iterations: iterations
		stored_key: mechanism.hash(client_key)
		server_key: mechanism.hmac(salted, 'Server Key'.bytes())
	}
}

// new_credentials derives the credentials a server stores for a new user,
// with a freshly generated random salt of `default_salt_size` bytes and
// `default_iterations` iterations.
//
// Example:
// ```v
// credentials := scram.new_credentials(.sha256, 'pencil')!
// assert credentials.salt.len == scram.default_salt_size
// assert credentials.iterations == scram.default_iterations
// ```
pub fn new_credentials(mechanism Mechanism, password string) !Credentials {
	salt := rand.bytes(default_salt_size)!
	return derive_credentials(mechanism, password, salt, default_iterations)!
}

// encode renders the credentials in the storage format of RFC 5803, laid out
// in the `authPassword` syntax of RFC 3112 §2:
//
//     <mechanism> "$" <iterations> ":" <salt> "$" <stored key> ":" <server key>
//
// with the three binary fields in base64. It is the format PostgreSQL keeps
// in `pg_authid.rolpassword`, so a record written here can be read by an
// LDAP directory or a PostgreSQL server, and vice versa.
//
// The result is a secret: `server_key` lets its holder impersonate the
// server to any client of this user.
//
// Example:
// ```v
// credentials := scram.derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(),
// 	4096)!
// encoded := credentials.encode()
// assert encoded.starts_with('SCRAM-SHA-256\$4096:')
// assert scram.parse_credentials(encoded)!.stored_key == credentials.stored_key
// ```
pub fn (c Credentials) encode() string {
	return '${c.mechanism.name()}\$${c.iterations}:${base64.encode(c.salt)}\$${base64.encode(c.stored_key)}:${base64.encode(c.server_key)}'
}

// parse_credentials reads back what `Credentials.encode` wrote, and with it
// any RFC 5803 record. Every field is validated, including the key lengths
// against the mechanism, so a truncated or hand-edited record is rejected
// rather than silently producing an account nobody can log into.
pub fn parse_credentials(encoded string) !Credentials {
	fields := encoded.split('\$')
	if fields.len != 3 {
		return MalformedMessage{
			reason: 'a stored credential must be `mechanism\$info\$value`'
		}
	}
	if fields[0].ends_with('-PLUS') {
		// RFC 5803 §2: the stored scheme names the hash, never the channel
		// binding variant, since the two share the same key material.
		return MalformedMessage{
			reason: 'a stored credential must not name a `-PLUS` mechanism'
		}
	}
	mechanism := mechanism_from_name(fields[0])!
	info := fields[1].split(':')
	value := fields[2].split(':')
	if info.len != 2 || value.len != 2 {
		return MalformedMessage{
			reason: 'a stored credential must be `mechanism\$iterations:salt\$stored:server`'
		}
	}
	iterations := parse_positive_int(info[0], 'iteration count')!
	salt := decode_base64(info[1], 'the salt')!
	if salt.len == 0 {
		return MalformedMessage{
			reason: 'the salt must not be empty'
		}
	}
	stored_key := decode_base64(value[0], 'the stored key')!
	server_key := decode_base64(value[1], 'the server key')!
	if stored_key.len != mechanism.size() || server_key.len != mechanism.size() {
		return MalformedMessage{
			reason: 'a ${mechanism.name()} credential needs ${mechanism.size()} byte keys, got ${stored_key.len} and ${server_key.len}'
		}
	}
	return Credentials{
		mechanism:  mechanism
		salt:       salt
		iterations: iterations
		stored_key: stored_key
		server_key: server_key
	}
}

// generate_nonce returns a fresh base64 nonce drawn from the CSPRNG. Every
// character of the base64 alphabet is a legal SCRAM `printable` character,
// so the result never needs escaping.
fn generate_nonce() !string {
	return base64.encode(rand.bytes(nonce_size)!)
}

// xor returns the byte-wise exclusive or of two equal length slices, which is
// how a proof is built from a key and a signature, and how the key is
// recovered from the proof on the other side.
fn xor(a []u8, b []u8) []u8 {
	mut out := []u8{len: a.len}
	for i in 0 .. a.len {
		out[i] = a[i] ^ b[i]
	}
	return out
}

// validate_nonce rejects nonces that would break the message grammar. SCRAM
// nonces are `printable` (RFC 5802 §7): US-ASCII 0x21-0x7E except the comma,
// which separates attributes.
fn validate_nonce(nonce string) ! {
	if nonce == '' {
		return error('scram: the nonce must not be empty')
	}
	for c in nonce {
		if c < 0x21 || c > 0x7e || c == `,` {
			return error('scram: the nonce must only contain printable US-ASCII characters other than a comma')
		}
	}
}
