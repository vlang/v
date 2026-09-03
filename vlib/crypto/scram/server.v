// The server half of a SCRAM exchange: two calls, one per message the
// server has to answer.
module scram

import crypto.subtle
import encoding.base64

// ServerConfig configures one server-side exchange. `lookup` is mandatory:
// it is how the server obtains the credentials it stored for a user.
@[params]
pub struct ServerConfig {
pub:
	// mechanism selects the hash. It must match the mechanism the credentials
	// returned by `lookup` were derived with.
	mechanism Mechanism = .sha256
	// channel_binding describes what *this exchange* uses. Set `mode` to
	// `.required` when the client picked the `-PLUS` mechanism, and leave it
	// at the default when it picked the base one.
	channel_binding ChannelBinding
	// advertises_plus says whether this server offers a `-PLUS` mechanism at
	// all, which is a different question from what this exchange uses: a
	// server commonly lists both `SCRAM-SHA-256` and `SCRAM-SHA-256-PLUS` and
	// lets the client choose. Set it to true whenever the `-PLUS` name is in
	// the advertised list, including on the exchanges where the client chose
	// the base mechanism — that is precisely where a stripped advertisement
	// has to be detected. `mode: .required` implies it.
	advertises_plus bool
	// nonce overrides the generated server nonce. Leave it empty outside of
	// tests.
	nonce string
	// lookup returns the credentials stored for `username`, which arrives
	// already unescaped.
	//
	// Returning an error aborts the exchange; the caller should then answer
	// `server_error_message('unknown-user')`. Note that doing so tells an
	// attacker which user names exist. RFC 5802 §7 suggests answering unknown
	// users with credentials derived from a server-side secret instead, so
	// that they are indistinguishable from a wrong password — that policy
	// belongs to the application, which is why it lives in this callback.
	lookup fn (username string) !Credentials @[required]
}

// ServerState tracks which message the server expects next.
enum ServerState {
	awaiting_client_first
	awaiting_client_final
	done
	// failed is entered by any step that does not run to completion, so that a
	// rejected client cannot try another proof against the same nonce.
	failed
}

// Server drives the server side of one SCRAM exchange. Create it with
// `new_server`, then call `first` and `final` in that order. It is not safe
// to share between threads; use one value per connection.
@[heap]
pub struct Server {
	mechanism       Mechanism
	channel_binding ChannelBinding
	advertises_plus bool
	server_nonce    string
	lookup          fn (username string) !Credentials = unsafe { nil }
mut:
	username     string
	authzid      string
	gs2_header   string
	nonce        string
	credentials  Credentials
	auth_message string
	state        ServerState = .awaiting_client_first
}

// new_server creates a server for one SCRAM exchange.
//
// Example:
// ```v
// creds := scram.new_credentials(.sha256, 'pencil')!
// mut server := scram.new_server(lookup: fn [creds] (username string) !scram.Credentials {
// 	return creds
// })!
// assert server.mechanism_name() == 'SCRAM-SHA-256'
// ```
pub fn new_server(config ServerConfig) !&Server {
	nonce := if config.nonce == '' { generate_nonce()! } else { config.nonce }
	validate_nonce(nonce)!
	// Rendering the header once here surfaces an incomplete channel binding
	// configuration at construction time rather than mid-exchange.
	config.channel_binding.gs2_flag()!
	return &Server{
		mechanism:       config.mechanism
		channel_binding: config.channel_binding
		advertises_plus: config.advertises_plus || config.channel_binding.mode == .required
		server_nonce:    nonce
		lookup:          config.lookup
	}
}

// str renders a Server without its secrets, for the same reason as
// `Client.str`: the credentials it holds must not reach a log through an
// incidental `println`.
pub fn (s &Server) str() string {
	return 'scram.Server{ mechanism: ${s.mechanism_name()}, username: ${s.username}, state: ${s.state} }'
}

// mechanism_name returns the SASL mechanism name this server implements,
// which is the `-PLUS` spelling when it requires channel binding.
pub fn (s &Server) mechanism_name() string {
	if s.channel_binding.mode == .required {
		return s.mechanism.name_plus()
	}
	return s.mechanism.name()
}

// username returns the authentication identity the client sent, unescaped.
// It is only meaningful once `first` has returned, and only trustworthy once
// `final` has succeeded.
pub fn (s &Server) username() string {
	return s.username
}

// authzid returns the authorization identity the client asked for, or an
// empty string when it did not ask for one. Authorizing it is the
// application's job: SCRAM only proves who the client is, never what it may
// act as.
pub fn (s &Server) authzid() string {
	return s.authzid
}

// done reports whether the exchange finished successfully.
pub fn (s &Server) done() bool {
	return s.state == .done
}

// server_error_message renders a server-final-message that refuses the
// exchange, as defined by RFC 5802 §7. Common codes are `invalid-proof`,
// `unknown-user` and `invalid-encoding`; a client surfaces the value as a
// `ServerError`.
pub fn server_error_message(code string) string {
	if code == '' || code.contains(',') {
		return 'e=other-error'
	}
	return 'e=${code}'
}

// first consumes the client-first-message and returns the
// server-first-message, which carries the user's salt, the iteration count
// and the combined nonce.
pub fn (mut s Server) first(client_first string) !string {
	if s.state != .awaiting_client_first {
		return error('scram: first() must be called exactly once, with the client-first-message')
	}
	s.state = .failed
	gs2_header, authzid, bare := split_gs2_header(client_first)!
	s.check_cbind_flag(gs2_header)!
	attrs := parse_attributes(bare)!
	if attrs[0].key == `m` {
		return MalformedMessage{
			reason: 'the client requires the unsupported mandatory extension `${attrs[0].value}`'
		}
	}
	if attrs.len < 2 || attrs[0].key != `n` || attrs[1].key != `r` {
		return MalformedMessage{
			reason: 'the client-first-message must be `n=`, then `r=`'
		}
	}
	username := unescape_saslname(attrs[0].value)!
	if username == '' {
		return MalformedMessage{
			reason: 'the user name must not be empty'
		}
	}
	client_nonce := attrs[1].value
	validate_nonce(client_nonce) or {
		return MalformedMessage{
			reason: 'the client nonce is not a printable string'
		}
	}
	credentials := s.lookup(username)!
	if credentials.mechanism != s.mechanism {
		return error('scram: the stored credentials for `${username}` are for ${credentials.mechanism.name()}, but this server speaks ${s.mechanism.name()}')
	}
	if credentials.salt.len == 0 || credentials.iterations < 1 {
		return error('scram: the stored credentials for `${username}` are incomplete')
	}
	// Keys of the wrong length would fail the proof check further down, which
	// reports a wrong password: a misconfigured store must not be diagnosed as
	// a user typing the wrong thing.
	if credentials.stored_key.len != s.mechanism.size()
		|| credentials.server_key.len != s.mechanism.size() {
		return error('scram: the stored credentials for `${username}` hold ${credentials.stored_key.len} and ${credentials.server_key.len} byte keys, but ${s.mechanism.name()} needs ${s.mechanism.size()}')
	}

	s.username = username
	s.authzid = authzid
	s.gs2_header = gs2_header
	s.credentials = credentials
	s.nonce = '${client_nonce}${s.server_nonce}'
	server_first := 'r=${s.nonce},s=${base64.encode(credentials.salt)},i=${credentials.iterations}'
	s.auth_message = '${bare},${server_first}'
	s.state = .awaiting_client_final
	return server_first
}

// final consumes the client-final-message, checks the client's proof and
// returns the server-final-message. An `AuthenticationFailed` here means a
// wrong password; answer the client with
// `server_error_message('invalid-proof')` rather than closing silently, so
// that it can tell a refusal from a broken connection.
pub fn (mut s Server) final(client_final string) !string {
	if s.state != .awaiting_client_final {
		return error('scram: final() must be called once, after first(), with the client-final-message')
	}
	s.state = .failed
	attrs := parse_attributes(client_final)!
	if attrs.len < 3 || attrs[0].key != `c` || attrs[1].key != `r` {
		return MalformedMessage{
			reason: 'the client-final-message must be `c=`, then `r=`, then `p=`'
		}
	}
	last := attrs[attrs.len - 1]
	if last.key != `p` {
		return MalformedMessage{
			reason: 'the client-final-message must end with `p=`'
		}
	}
	// The client echoes the GS2 header it actually sent. Comparing it with the
	// one received in the client-first-message is what makes a rewritten
	// header — a stripped channel binding, an injected authzid — detectable.
	expected_cbind := base64.encode(s.channel_binding.cbind_input(s.gs2_header))
	if attrs[0].value != expected_cbind {
		return AuthenticationFailed{
			reason: 'the channel binding data does not match the GS2 header of the client-first-message'
		}
	}
	if attrs[1].value != s.nonce {
		return AuthenticationFailed{
			reason: 'the client nonce does not match the one of this exchange'
		}
	}
	proof := decode_base64(last.value, 'the client proof')!
	if proof.len != s.mechanism.size() {
		return AuthenticationFailed{
			reason: 'the client proof is ${proof.len} bytes, expected ${s.mechanism.size()}'
		}
	}

	without_proof := client_final#[..-(last.value.len + 3)]
	auth_message := '${s.auth_message},${without_proof}'
	client_signature := s.mechanism.hmac(s.credentials.stored_key, auth_message.bytes())
	// ClientKey is recovered from the proof, then hashed: the result must be
	// the StoredKey, which is all the server keeps. See RFC 5802 §3.
	client_key := xor(proof, client_signature)
	if subtle.constant_time_compare(s.mechanism.hash(client_key), s.credentials.stored_key) != 1 {
		return AuthenticationFailed{
			reason: 'the client proof does not match the stored key'
		}
	}
	s.state = .done
	signature := s.mechanism.hmac(s.credentials.server_key, auth_message.bytes())
	return 'v=${base64.encode(signature)}'
}

// check_cbind_flag compares the GS2 `cbind-flag` the client sent with what
// this server offers. The `y` case is the downgrade check of RFC 5802 §6: a
// client only sends it when the server advertised no `-PLUS` mechanism, so a
// server that did advertise one knows the list was tampered with. Which is
// why the check reads `advertises_plus` and not the mode: on an exchange
// that runs the base mechanism the mode says nothing about the list.
fn (s &Server) check_cbind_flag(gs2_header string) ! {
	flag := gs2_header.all_before(',')
	match s.channel_binding.mode {
		.required {
			if flag != 'p=${s.channel_binding.name}' {
				return AuthenticationFailed{
					reason: 'this server requires the `${s.channel_binding.name}` channel binding, but the client sent `${flag}`'
				}
			}
		}
		.not_supported, .unsupported_by_server {
			if flag.starts_with('p=') {
				return MalformedMessage{
					reason: 'the client asked for the `${flag#[2..]}` channel binding, which this server does not offer'
				}
			}
			if flag != 'n' && flag != 'y' {
				return MalformedMessage{
					reason: 'unknown GS2 channel binding flag `${flag}`'
				}
			}
			// `y` asserts something about the mechanism list this server sent,
			// and this server knows what it sent. `n` stays valid: a client
			// that cannot do channel binding at all is not being downgraded.
			if flag == 'y' && s.advertises_plus {
				return AuthenticationFailed{
					reason: 'the client claims this server advertises no -PLUS mechanism, but it does: the advertised list was altered in transit'
				}
			}
		}
	}
}

// split_gs2_header splits a client-first-message into its GS2 header, the
// authorization identity that header carries, and the
// client-first-message-bare that follows it.
fn split_gs2_header(client_first string) !(string, string, string) {
	flag_end := client_first.index(',') or {
		return MalformedMessage{
			reason: 'the client-first-message has no GS2 header'
		}
	}
	rest := client_first[flag_end + 1..]
	authzid_end := rest.index(',') or {
		return MalformedMessage{
			reason: 'the GS2 header of the client-first-message is incomplete'
		}
	}
	header := client_first[..flag_end + authzid_end + 2]
	authzid_part := rest[..authzid_end]
	mut authzid := ''
	if authzid_part != '' {
		if !authzid_part.starts_with('a=') {
			return MalformedMessage{
				reason: 'expected `a=` in the GS2 header, got `${authzid_part}`'
			}
		}
		authzid = unescape_saslname(authzid_part[2..])!
		// RFC 5802 §7 spells authzid as `a=` saslname, and saslname is `1*`:
		// an `a=` that carries nothing is malformed, not an absent authzid.
		if authzid == '' {
			return MalformedMessage{
				reason: 'the `a=` authorization identity of the GS2 header is empty'
			}
		}
	}
	return header, authzid, rest[authzid_end + 1..]
}
