// The client half of a SCRAM exchange: three calls, in order, one per
// message the client has to produce or check.
module scram

import crypto.subtle
import encoding.base64

// ClientConfig configures one client-side exchange. Only `username` and
// `password` are mandatory; the defaults give an RFC 7677 compliant
// `SCRAM-SHA-256` client without channel binding.
@[params]
pub struct ClientConfig {
pub:
	// username is the authentication identity. It is escaped as needed, so it
	// may contain commas and equals signs.
	username string @[required]
	// password is the secret. See the SASLprep note in the module README if it
	// may contain non-ASCII characters. It is required rather than defaulted,
	// so that an exchange is never attempted with an empty password by accident.
	password string @[required]
	// mechanism selects the hash. Prefer the default over `.sha1`, which
	// survives only for servers that offer nothing better.
	mechanism Mechanism = .sha256
	// authzid is the authorization identity, when it differs from `username`.
	// Leave it empty in the common case where a user authenticates as itself.
	authzid string
	// channel_binding binds the exchange to the TLS channel underneath it.
	channel_binding ChannelBinding
	// min_iterations is the smallest iteration count accepted from the server.
	// Lowering it below `default_min_iterations` weakens the protection an
	// intercepted exchange gets against an offline attack.
	min_iterations int = default_min_iterations
	// max_iterations is the largest iteration count accepted from the server.
	// Raising it lets a hostile server spend more of this client's CPU on a
	// single message; see `default_max_iterations`.
	max_iterations int = default_max_iterations
	// nonce overrides the generated client nonce. Leave it empty outside of
	// tests: a nonce that repeats across exchanges destroys replay protection.
	nonce string
}

// ClientState tracks which message the client expects next, so that steps
// called out of order fail loudly instead of producing a wrong exchange.
enum ClientState {
	awaiting_first
	awaiting_server_first
	awaiting_server_final
	done
	// failed is entered by any step that does not run to completion, so that a
	// caller which ignores an error cannot mistake a refused exchange for a
	// successful one, and cannot retry a step on the same nonce.
	failed
}

// Client drives the client side of one SCRAM exchange. Create it with
// `new_client`, then call `first`, `final` and `verify` in that order. It
// holds the password for the duration of the exchange and is not safe to
// share between threads.
@[heap]
pub struct Client {
	mechanism       Mechanism
	username        string
	password        string
	authzid         string
	channel_binding ChannelBinding
	min_iterations  int
	max_iterations  int
	gs2_header      string
	client_nonce    string
mut:
	first_bare   string
	auth_message string
	server_key   []u8
	state        ClientState = .awaiting_first
}

// new_client creates a client for one SCRAM exchange.
//
// Example:
// ```v
// mut client := scram.new_client(username: 'user', password: 'pencil')!
// assert client.mechanism_name() == 'SCRAM-SHA-256'
// ```
pub fn new_client(config ClientConfig) !&Client {
	if config.username == '' {
		return error('scram: the username must not be empty')
	}
	if config.min_iterations < 1 {
		return error('scram: min_iterations must be at least 1, got ${config.min_iterations}')
	}
	if config.max_iterations < config.min_iterations {
		return error('scram: max_iterations (${config.max_iterations}) must not be below min_iterations (${config.min_iterations})')
	}
	nonce := if config.nonce == '' { generate_nonce()! } else { config.nonce }
	validate_nonce(nonce)!
	return &Client{
		mechanism:       config.mechanism
		username:        config.username
		password:        config.password
		authzid:         config.authzid
		channel_binding: config.channel_binding
		min_iterations:  config.min_iterations
		max_iterations:  config.max_iterations
		gs2_header:      config.channel_binding.gs2_header(config.authzid)!
		client_nonce:    nonce
	}
}

// str renders a Client without its secrets. The password and the derived
// keys are deliberately left out: V formats structs automatically, so a
// `println(client)` while debugging would otherwise write the password to
// wherever the logs go.
pub fn (c &Client) str() string {
	return 'scram.Client{ mechanism: ${c.mechanism_name()}, username: ${c.username}, state: ${c.state} }'
}

// mechanism_name returns the SASL mechanism name to announce to the server,
// which is the `-PLUS` spelling when channel binding is in use.
pub fn (c &Client) mechanism_name() string {
	if c.channel_binding.mode == .required {
		return c.mechanism.name_plus()
	}
	return c.mechanism.name()
}

// done reports whether the exchange finished successfully, which is only true
// once `verify` has accepted the server-final-message.
pub fn (c &Client) done() bool {
	return c.state == .done
}

// first returns the client-first-message to send to the server. It carries no
// secret, only the user name and the client nonce.
pub fn (mut c Client) first() !string {
	if c.state != .awaiting_first {
		return error('scram: first() must be called exactly once, at the start of the exchange')
	}
	c.first_bare = 'n=${escape_saslname(c.username)},r=${c.client_nonce}'
	c.state = .awaiting_server_first
	return '${c.gs2_header}${c.first_bare}'
}

// final consumes the server-first-message and returns the
// client-final-message, which carries the proof that this client knows the
// password. The server's salt and iteration count are validated here, so a
// server can neither weaken the exchange by asking for a trivial amount of
// work, nor stall this client by asking for an absurd amount of it.
pub fn (mut c Client) final(server_first string) !string {
	if c.state != .awaiting_server_first {
		return error('scram: final() must be called once, after first(), with the server-first-message')
	}
	// Every early return below is a failure, so the state moves first and is
	// only advanced again once the message has been produced.
	c.state = .failed
	attrs := parse_attributes(server_first)!
	// RFC 5802 §5.1: `m=` announces an extension the client must understand.
	// This module understands none, so its presence has to end the exchange.
	if attrs[0].key == `m` {
		return MalformedMessage{
			reason: 'the server requires the unsupported mandatory extension `${attrs[0].value}`'
		}
	}
	if attrs.len < 3 || attrs[0].key != `r` || attrs[1].key != `s` || attrs[2].key != `i` {
		return MalformedMessage{
			reason: 'the server-first-message must be `r=`, then `s=`, then `i=`'
		}
	}
	nonce := attrs[0].value
	// The whole received value is checked, not just the part this client
	// contributed: it is echoed verbatim in the client-final-message and in
	// the auth message, so it has to satisfy the same `printable` rule the
	// server already applies to the client nonce. Extending a valid prefix
	// with spaces or control bytes is not a valid nonce.
	validate_nonce(nonce) or {
		return MalformedMessage{
			reason: 'the server nonce is not a printable string'
		}
	}
	// The server nonce must extend the client one. If it does not, the reply
	// belongs to another exchange, so it is a replay rather than a bad password.
	if !nonce.starts_with(c.client_nonce) || nonce.len == c.client_nonce.len {
		return AuthenticationFailed{
			reason: 'the server nonce does not extend the client nonce'
		}
	}
	salt := decode_base64(attrs[1].value, 'the salt')!
	if salt.len == 0 {
		return MalformedMessage{
			reason: 'the salt must not be empty'
		}
	}
	iterations := parse_positive_int(attrs[2].value, 'iteration count')!
	if iterations < c.min_iterations {
		return AuthenticationFailed{
			reason: 'the server asked for ${iterations} iterations, below the configured minimum of ${c.min_iterations}'
		}
	}
	// The ceiling is checked before `hi()` rather than after: the point is to
	// never start the work, since a server that names an absurd count is
	// spending this client's CPU rather than protecting its own passwords.
	if iterations > c.max_iterations {
		return AuthenticationFailed{
			reason: 'the server asked for ${iterations} iterations, above the configured maximum of ${c.max_iterations}'
		}
	}

	cbind := base64.encode(c.channel_binding.cbind_input(c.gs2_header))
	final_bare := 'c=${cbind},r=${nonce}'
	c.auth_message = '${c.first_bare},${server_first},${final_bare}'

	salted := c.mechanism.hi(c.password.bytes(), salt, iterations)
	client_key := c.mechanism.hmac(salted, 'Client Key'.bytes())
	stored_key := c.mechanism.hash(client_key)
	client_signature := c.mechanism.hmac(stored_key, c.auth_message.bytes())
	c.server_key = c.mechanism.hmac(salted, 'Server Key'.bytes())
	c.state = .awaiting_server_final
	return '${final_bare},p=${base64.encode(xor(client_key, client_signature))}'
}

// verify checks the server-final-message. It returns without a value when the
// server proved that it holds the credentials for this user, which is the
// point at which the connection may be trusted. A `ServerError` means the
// server refused the exchange; an `AuthenticationFailed` means it answered
// but could not sign the exchange, so it is not the server it claims to be.
pub fn (mut c Client) verify(server_final string) ! {
	if c.state != .awaiting_server_final {
		return error('scram: verify() must be called once, after final(), with the server-final-message')
	}
	c.state = .failed
	attrs := parse_attributes(server_final)!
	if attrs[0].key == `e` {
		return ServerError{
			code: attrs[0].value
		}
	}
	if attrs[0].key != `v` {
		return MalformedMessage{
			reason: 'the server-final-message must start with `v=` or `e=`'
		}
	}
	signature := decode_base64(attrs[0].value, 'the server signature')!
	expected := c.mechanism.hmac(c.server_key, c.auth_message.bytes())
	if subtle.constant_time_compare(signature, expected) != 1 {
		return AuthenticationFailed{
			reason: 'the server signature does not match'
		}
	}
	c.state = .done
}
