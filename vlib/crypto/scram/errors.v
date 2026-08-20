// Typed errors returned by this module. Callers can tell the three
// situations that matter apart without parsing strings: the peer sent
// something that is not a SCRAM message, the peer failed to prove what it
// claimed, or the exchange was refused. Programming errors — calling the
// steps out of order, passing an empty username — are returned as plain
// `error('...')` values, because there is nothing to branch on.
module scram

// MalformedMessage means the bytes received do not form a valid SCRAM
// message. This is a permanent error: retrying will not help, and it usually
// points at a peer that speaks a different protocol or a corrupted transport.
pub struct MalformedMessage {
	Error
pub:
	// reason names the check that failed.
	reason string
}

// msg formats a MalformedMessage for `IError.msg()`.
pub fn (e &MalformedMessage) msg() string {
	return 'scram: malformed message: ${e.reason}'
}

// AuthenticationFailed means the exchange was well formed but the peer did
// not prove what it claimed. On the server that means a wrong password; on
// the client it means the server could not sign the exchange, so it does not
// hold the credentials it should — treat the connection as hostile rather
// than retrying.
//
// It is deliberately not split into finer variants: telling a caller *why*
// authentication failed is exactly the information an attacker wants.
pub struct AuthenticationFailed {
	Error
pub:
	// reason is a short diagnostic for logs. Do not relay it to a remote peer.
	reason string
}

// msg formats an AuthenticationFailed for `IError.msg()`.
pub fn (e &AuthenticationFailed) msg() string {
	return 'scram: authentication failed: ${e.reason}'
}

// UnsupportedMechanism means a mechanism name is not one this module
// implements. It is returned by `mechanism_from_name`, typically while
// picking a mechanism out of the list a server advertises.
pub struct UnsupportedMechanism {
	Error
pub:
	// name is the mechanism name that was not recognised.
	name string
}

// msg formats an UnsupportedMechanism for `IError.msg()`.
pub fn (e &UnsupportedMechanism) msg() string {
	return 'scram: unsupported mechanism: ${e.name}'
}

// ServerError carries the `e=` attribute of a server-final-message, which is
// how a server reports a refusal instead of signing the exchange. RFC 5802
// §7 defines the values, `invalid-proof` and `unknown-user` being the common
// ones, but a server may send any token, so do not assume the set is closed.
pub struct ServerError {
	Error
pub:
	// code is the `server-error-value` sent by the server, verbatim.
	code string
}

// msg formats a ServerError for `IError.msg()`.
pub fn (e &ServerError) msg() string {
	return 'scram: server refused the exchange: ${e.code}'
}
