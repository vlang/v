// The SASL PLAIN mechanism (RFC 4616): a single client message carrying an
// optional authorization identity, an authentication identity and a
// password, separated by NUL bytes.
module sasl

import encoding.utf8

// PlainMessage holds the fields of a SASL PLAIN message.
@[params]
pub struct PlainMessage {
pub:
	// authzid is the identity to act as. Leave it empty to act as `authcid`.
	// Its form is defined by the SASL profile of the application protocol.
	authzid string
	// authcid is the identity whose password is checked. It must not be empty.
	authcid string
	// password is the clear-text password. It must not be empty.
	password string
}

// str renders a PlainMessage without its password, so that logging a parsed
// message does not leak it.
pub fn (m PlainMessage) str() string {
	return "sasl.PlainMessage{ authzid: '${m.authzid}', authcid: '${m.authcid}', password: <redacted> }"
}

// encode_plain returns the SASL PLAIN message for `m`: `authzid`, NUL,
// `authcid`, NUL, `password` (RFC 4616, section 2). The result is the raw
// message; base64-encode it when the protocol requires it, as SMTP, IMAP and
// IRC do.
//
// It returns an error when `authcid` or `password` is empty, or when a field
// contains a NUL byte or is not valid UTF-8. The fields are sent unchanged.
// Preparing `authcid` and `password` is the server's job; prepare them here
// only in the way agreed with the server. The form of `authzid` follows the
// SASL profile of the application protocol.
pub fn encode_plain(m PlainMessage) !string {
	check_plain_field('authzid', m.authzid)!
	check_plain_field('authcid', m.authcid)!
	check_plain_field('password', m.password)!
	if m.authcid == '' {
		return error('sasl: the PLAIN authcid must not be empty')
	}
	if m.password == '' {
		return error('sasl: the PLAIN password must not be empty')
	}
	return '${m.authzid}\x00${m.authcid}\x00${m.password}'
}

// parse_plain splits a SASL PLAIN message into its fields. `message` is the
// raw message, after any base64 decoding done by the transport.
//
// It returns an error unless the message has exactly two NUL separators, a
// non-empty `authcid` and `password`, and is valid UTF-8. The error never
// includes the message content. An empty `authzid` in the result means that
// the client sent none.
//
// It does not authenticate anything, and it checks the fields as received,
// before any preparation. The server still has to prepare `authcid` and
// `password` (RFC 4616 recommends SASLprep) and fail if the preparation fails
// or leaves either of them empty, check the password against its own store,
// and decide whether `authcid` may act as a non-empty `authzid`.
// Servers must accept fields of up to 255 bytes each; bound the total size of
// the message in the transport.
pub fn parse_plain(message string) !PlainMessage {
	if message.count('\x00') != 2 {
		return error('sasl: the PLAIN message must have exactly two NUL separators')
	}
	if !utf8.validate_str(message) {
		return error('sasl: the PLAIN message is not valid UTF-8')
	}
	first := message.index_u8(0)
	second := message.last_index_u8(0)
	m := PlainMessage{
		authzid:  message[..first]
		authcid:  message[first + 1..second]
		password: message[second + 1..]
	}
	if m.authcid == '' {
		return error('sasl: the PLAIN authcid is empty')
	}
	if m.password == '' {
		return error('sasl: the PLAIN password is empty')
	}
	return m
}

fn check_plain_field(name string, value string) ! {
	if value.contains_u8(0) {
		return error('sasl: the PLAIN ${name} must not contain a NUL byte')
	}
	if !utf8.validate_str(value) {
		return error('sasl: the PLAIN ${name} is not valid UTF-8')
	}
}
