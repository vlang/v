// Typed errors surfaced by the signature module. All concrete error
// types embed `Error` so callers can pattern-match with `err is X` (V's
// idiomatic typed-error matching).
module signature

// VerificationFailed is returned when a signature does not match the
// recomputed signature base. Distinct from `MalformedMessage` so that
// callers can tell "wire bytes are fine but signature is bad" apart
// from "wire bytes are unparseable".
pub struct VerificationFailed {
	Error
pub:
	label string
}

// msg formats a VerificationFailed for `IError.msg()`.
pub fn (e &VerificationFailed) msg() string {
	if e.label != '' {
		return 'http.signature: signature ${e.label} did not verify'
	}
	return 'http.signature: signature did not verify'
}

// MalformedMessage covers all syntactic and structural problems with
// the inputs (missing covered components, malformed Signature-Input,
// unknown derived component, etc.). The `reason` field carries the
// short human-readable detail.
pub struct MalformedMessage {
	Error
pub:
	reason string
}

// msg formats a MalformedMessage for `IError.msg()`.
pub fn (e &MalformedMessage) msg() string {
	return 'http.signature: ${e.reason}'
}

// UnsupportedAlgorithm is returned when the `alg` parameter (or the
// implied algorithm of the supplied key) is not one this module
// implements. Carrying the offending token lets callers report it
// clearly rather than echoing a generic "not supported".
pub struct UnsupportedAlgorithm {
	Error
pub:
	name string
}

// msg formats an UnsupportedAlgorithm for `IError.msg()`.
pub fn (e &UnsupportedAlgorithm) msg() string {
	return 'http.signature: algorithm ${e.name} is not supported'
}

// SignatureExpired is returned by verification helpers when the
// signature's `expires` parameter is at or before the verification
// time. Callers that don't want this check can pass their own time
// reference (or use the lower-level `verify` that does no time check).
pub struct SignatureExpired {
	Error
pub:
	expires i64
	now     i64
}

// msg formats a SignatureExpired for `IError.msg()`.
pub fn (e &SignatureExpired) msg() string {
	return 'http.signature: signature expired (expires=${e.expires}, now=${e.now})'
}
