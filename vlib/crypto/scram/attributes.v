// Message grammar helpers: the GS2 header that prefixes the
// client-first-message, the escaping rules for user names, and the
// comma-separated `key=value` list every SCRAM message is made of.
// See RFC 5802 §7 for the ABNF these follow.
module scram

import encoding.base64

// ChannelBindingMode says how an exchange relates to the TLS channel
// underneath it, and becomes the GS2 `cbind-flag` on the wire. The
// distinction between `.not_supported` and `.unsupported_by_server` is not
// cosmetic: it is what lets a server detect an attacker stripping the
// `-PLUS` mechanisms from its advertised list.
pub enum ChannelBindingMode {
	// not_supported sends `n`: this client cannot do channel binding at all.
	not_supported
	// unsupported_by_server sends `y`: this client supports channel binding,
	// but the server did not advertise a `-PLUS` mechanism. A server that did
	// advertise one must then abort, since only a downgrade attack explains it.
	unsupported_by_server
	// required sends `p=<name>`: the exchange is bound to the channel, and
	// both sides must agree on `name` and `data`. Use the `-PLUS` mechanism
	// name in that case.
	required
}

// ChannelBinding describes the channel binding of an exchange. The default
// value means no channel binding, which is what a client without access to
// its TLS layer should use.
//
// This module never derives binding data itself: `data` comes from the TLS
// stack, and the caller passes it in. For `tls-server-end-point` (RFC 5929)
// it is the hash of the server certificate; for `tls-exporter` (RFC 9266) it
// is a TLS exporter output.
pub struct ChannelBinding {
pub:
	// mode selects the GS2 `cbind-flag`.
	mode ChannelBindingMode = .not_supported
	// name is the channel binding type, e.g. `tls-server-end-point`. Required
	// when `mode` is `.required`, ignored otherwise.
	name string
	// data is the binding data from the TLS layer. Required when `mode` is
	// `.required`, ignored otherwise.
	data []u8
}

// gs2_flag renders the GS2 `cbind-flag` of RFC 5802 §7.
fn (cb ChannelBinding) gs2_flag() !string {
	match cb.mode {
		.not_supported {
			return 'n'
		}
		.unsupported_by_server {
			return 'y'
		}
		.required {
			if cb.name == '' {
				return error('scram: a channel binding name is required when mode is .required')
			}
			if cb.name.contains(',') || cb.name.contains('=') {
				return error('scram: the channel binding name must not contain a comma or an equals sign')
			}
			return 'p=${cb.name}'
		}
	}
}

// gs2_header renders the full GS2 header of RFC 5802 §7, which prefixes the
// client-first-message and is repeated, base64 encoded, in the `c=` attribute
// of the client-final-message so that it cannot be tampered with in transit.
fn (cb ChannelBinding) gs2_header(authzid string) !string {
	flag := cb.gs2_flag()!
	if authzid == '' {
		return '${flag},,'
	}
	return '${flag},a=${escape_saslname(authzid)},'
}

// cbind_input returns the value the `c=` attribute base64 encodes: the GS2
// header, followed by the binding data when channel binding is in use.
fn (cb ChannelBinding) cbind_input(gs2_header string) []u8 {
	mut out := gs2_header.bytes()
	if cb.mode == .required {
		out << cb.data
	}
	return out
}

// escape_saslname applies the `saslname` escaping of RFC 5802 §5.1. A comma
// would end the attribute, so it becomes `=2C`; the equals sign that
// introduces the escape becomes `=3D`.
fn escape_saslname(name string) string {
	if !name.contains_any('=,') {
		return name
	}
	mut out := []u8{cap: name.len + 8}
	for c in name {
		if c == `=` {
			out << '=3D'.bytes()
		} else if c == `,` {
			out << '=2C'.bytes()
		} else {
			out << c
		}
	}
	return out.bytestr()
}

// unescape_saslname reverses `escape_saslname`. Any other `=` sequence is
// invalid per RFC 5802 §5.1 and is rejected rather than passed through, so a
// peer cannot smuggle a name past a check by inventing an escape.
fn unescape_saslname(name string) !string {
	if !name.contains('=') {
		if name.contains(',') {
			return MalformedMessage{
				reason: 'unescaped comma in a user name'
			}
		}
		return name
	}
	mut out := []u8{cap: name.len}
	mut i := 0
	for i < name.len {
		c := name[i]
		if c == `,` {
			return MalformedMessage{
				reason: 'unescaped comma in a user name'
			}
		}
		if c != `=` {
			out << c
			i++
			continue
		}
		if i + 3 > name.len {
			return MalformedMessage{
				reason: 'truncated escape sequence in a user name'
			}
		}
		match name[i..i + 3] {
			'=2C' {
				out << u8(`,`)
			}
			'=3D' {
				out << u8(`=`)
			}
			else {
				return MalformedMessage{
					reason: 'invalid escape sequence `${name[i..i + 3]}` in a user name'
				}
			}
		}
		i += 3
	}
	return out.bytestr()
}

// Attribute is one `key=value` pair of a SCRAM message. Keys are single
// letters, which is why `key` is a `u8` rather than a string.
struct Attribute {
	key   u8
	value string
}

// parse_attributes splits a SCRAM message into its attributes. Values are
// returned verbatim; a comma always separates attributes, because the
// grammar of RFC 5802 §7 excludes it from every value.
fn parse_attributes(message string) ![]Attribute {
	if message == '' {
		return MalformedMessage{
			reason: 'empty message'
		}
	}
	parts := message.split(',')
	mut out := []Attribute{cap: parts.len}
	for part in parts {
		if part.len < 3 || part[1] != `=` {
			return MalformedMessage{
				reason: 'expected a `key=value` attribute, got `${part}`'
			}
		}
		if !part[0].is_letter() {
			return MalformedMessage{
				reason: 'attribute names must be letters, got `${part[0].ascii_str()}`'
			}
		}
		out << Attribute{
			key:   part[0]
			value: part[2..]
		}
	}
	return out
}

// find returns the value of the first attribute named `key`.
fn (attrs []Attribute) find(key u8) ?string {
	for attr in attrs {
		if attr.key == key {
			return attr.value
		}
	}
	return none
}

// decode_base64 decodes a base64 attribute value, rejecting anything that is
// not canonical base64. `base64.decode` reports no error of its own, so the
// check is a round trip: only a canonical encoding survives it unchanged.
fn decode_base64(value string, what string) ![]u8 {
	decoded := base64.decode(value)
	if base64.encode(decoded) != value {
		return MalformedMessage{
			reason: '${what} is not valid base64'
		}
	}
	return decoded
}

// parse_positive_int parses an unsigned decimal attribute value. RFC 5802 §7
// spells the iteration count as a `posit-number`, which forbids a sign, a
// leading zero and any surrounding space, so the parse is deliberately strict
// rather than using the lenient `string.int()`.
fn parse_positive_int(value string, what string) !int {
	if value == '' || value.len > 9 {
		return MalformedMessage{
			reason: 'invalid ${what} `${value}`'
		}
	}
	if value[0] == `0` {
		return MalformedMessage{
			reason: 'invalid ${what} `${value}`: leading zeroes are not allowed'
		}
	}
	mut n := 0
	for c in value {
		if c < `0` || c > `9` {
			return MalformedMessage{
				reason: 'invalid ${what} `${value}`'
			}
		}
		n = n * 10 + int(c - `0`)
	}
	return n
}
