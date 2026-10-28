module veb

import crypto.hmac
import crypto.sha256
import encoding.base64
import net.http
import strings
import time

// Signed-cookie sessions for veb, inspired by the framework-agnostic
// einar-hjortdal/sessions library. All state lives in the cookie itself:
// `payload_b64.HMAC_SHA256(secret, payload_b64)`, so the server stores
// nothing. Values must already be strings (encode structs to json first).

// SessionOptions configures a signed-cookie session.
@[params]
pub struct SessionOptions {
pub:
	// name is the cookie name. Each name is an independent session.
	name string = 'veb_session'
	// secret signs the cookie. It must be non-empty and unpredictable;
	// an empty secret refuses to load and to save anything.
	secret    string @[required]
	max_age   int    = 24 * 60 * 60
	path      string = '/'
	secure    bool
	http_only bool = true
}

// Session is a set of name/value pairs persisted in a signed cookie.
// Get one from `Context.session`, mutate it with `set`, persist it with
// `Context.save_session`.
pub struct Session {
mut:
	values map[string]string
	opts   SessionOptions
}

// session loads the named session from the request cookies, or returns a new
// empty one when absent, tampered with, expired, or the secret is empty.
pub fn (mut ctx Context) session(opts SessionOptions) Session {
	mut s := Session{
		opts: opts
	}
	if opts.secret == '' {
		return s
	}
	if raw := ctx.get_cookie(opts.name) {
		if decoded := decode_session_cookie(raw, opts.secret) {
			if decoded.exp >= time.now().unix() {
				s.values = decoded.values
			}
		}
	}
	return s
}

// set stores a value in the session. Call save_session to persist it.
pub fn (mut s Session) set(key string, value string) {
	s.values[key] = value
}

// get returns a session value, or none when the key is absent.
pub fn (s Session) get(key string) ?string {
	if key in s.values {
		return s.values[key]
	}
	return none
}

// save_session persists the session in a signed cookie on the response.
// An empty secret saves nothing.
pub fn (mut ctx Context) save_session(s Session) {
	if s.opts.secret == '' {
		return
	}
	exp := time.now().unix() + i64(s.opts.max_age)
	value := encode_session_cookie(s.values, exp, s.opts.secret)
	ctx.set_cookie(http.Cookie{
		name:      s.opts.name
		value:     value
		path:      s.opts.path
		max_age:   s.opts.max_age
		secure:    s.opts.secure
		http_only: s.opts.http_only
	})
}

// clear_session deletes the named session cookie on the response.
pub fn (mut ctx Context) clear_session(opts SessionOptions) {
	ctx.set_cookie(http.Cookie{
		name:      opts.name
		value:     ''
		path:      opts.path
		max_age:   -1
		secure:    opts.secure
		http_only: opts.http_only
	})
}

struct DecodedSession {
	values map[string]string
	exp    i64
}

fn encode_session_cookie(values map[string]string, exp i64, secret string) string {
	payload_b64 := base64.url_encode_str(encode_session_values(values, exp))
	mac := hmac.new(secret.bytes(), payload_b64.bytes(), sha256.sum, 64)
	return payload_b64 + '.' + base64.url_encode(mac)
}

fn decode_session_cookie(token string, secret string) ?DecodedSession {
	parts := token.split('.')
	if parts.len != 2 {
		return none
	}
	payload_b64 := parts[0]
	mac := decode_base64url(parts[1]) or { return none }
	expected := hmac.new(secret.bytes(), payload_b64.bytes(), sha256.sum, 64)
	if !hmac.equal(mac, expected) {
		return none
	}
	payload := decode_base64url_str(payload_b64) or { return none }
	return decode_session_values(payload)
}

// decode_base64url decodes unpadded base64url, rejecting anything outside
// the alphabet first: the stdlib decoder silently maps invalid digits.
fn decode_base64url(s string) ?[]u8 {
	if !is_base64url_string(s) {
		return none
	}
	return base64.url_decode(s)
}

// decode_base64url_str is the string variant of decode_base64url.
fn decode_base64url_str(s string) ?string {
	if !is_base64url_string(s) {
		return none
	}
	return base64.url_decode_str(s)
}

fn is_base64url_string(s string) bool {
	if s == '' {
		return false
	}
	for c in s.bytes() {
		if c == `-` || c == `_` {
			continue
		}
		if c.is_digit() {
			continue
		}
		if (c >= `A` && c <= `Z`) || (c >= `a` && c <= `z`) {
			continue
		}
		return false
	}
	return true
}

fn encode_session_values(values map[string]string, exp i64) string {
	mut sb := strings.new_builder(values.len * 48 + 24)
	sb.write_string(exp.str())
	sb.write_string('\n')
	for key, value in values {
		sb.write_string(base64.url_encode_str(key))
		sb.write_string('=')
		sb.write_string(base64.url_encode_str(value))
		sb.write_string('\n')
	}
	return sb.str()
}

fn decode_session_values(payload string) ?DecodedSession {
	lines := payload.split('\n')
	if lines.len == 0 {
		return none
	}
	exp := lines[0].i64()
	if exp <= 0 {
		return none
	}
	mut values := map[string]string{}
	for line in lines[1..] {
		if line == '' {
			continue
		}
		kv := line.split_nth('=', 2)
		if kv.len != 2 {
			return none
		}
		// Empty keys and values are valid map entries and encode as empty strings.
		key := if kv[0] == '' { '' } else { decode_base64url_str(kv[0]) or { return none } }
		value := if kv[1] == '' { '' } else { decode_base64url_str(kv[1]) or { return none } }
		values[key] = value
	}
	return DecodedSession{
		values: values
		exp:    exp
	}
}
