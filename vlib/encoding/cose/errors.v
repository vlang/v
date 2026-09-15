// Typed errors returned by this module. Callers can `match` on these
// variants (after `if err is X`) to react programmatically. Free-form
// errors continue to be returned via `error('...')` for diagnostic
// strings.
module cose

// VerificationFailed is returned by verify routines when the signature
// or MAC tag does not match. Distinct from MalformedMessage so callers
// can tell a tampered/wrong-key message apart from a structurally
// invalid one.
pub struct VerificationFailed {
	Error
pub:
	// algorithm is the algorithm that was attempted, if known.
	algorithm ?Algorithm
}

// msg formats a VerificationFailed for `IError.msg()`.
pub fn (e &VerificationFailed) msg() string {
	if alg := e.algorithm {
		return 'cose: verification failed (${alg.name()})'
	}
	return 'cose: verification failed'
}

// MalformedMessage indicates the input bytes do not form a valid COSE
// message of the expected type. The `reason` describes which check
// failed; callers should treat this as a permanent error.
pub struct MalformedMessage {
	Error
pub:
	reason string
}

// msg formats a MalformedMessage for `IError.msg()`.
pub fn (e &MalformedMessage) msg() string {
	return 'cose: malformed message: ${e.reason}'
}

// AlgorithmMismatch is returned when a key constrains itself to one
// algorithm via its `alg` parameter and the caller asks the module
// to use a different one. This catches the common mistake of passing
// e.g. an ES256-only key to an EdDSA signing call.
pub struct AlgorithmMismatch {
	Error
pub:
	expected Algorithm // algorithm declared by the key
	got      Algorithm // algorithm requested for the operation
}

// msg formats an AlgorithmMismatch for `IError.msg()`.
pub fn (e &AlgorithmMismatch) msg() string {
	return 'cose: key declares alg=${e.expected.name()} but operation requested ${e.got.name()}'
}

// UnsupportedAlgorithm is returned when an operation is attempted with
// an algorithm that the called function does not handle (e.g. a MAC
// algorithm passed to a signing routine).
pub struct UnsupportedAlgorithm {
	Error
pub:
	algorithm Algorithm
	context   string // e.g. "signing", "MAC"
}

// msg formats an UnsupportedAlgorithm for `IError.msg()`.
pub fn (e &UnsupportedAlgorithm) msg() string {
	return 'cose: ${e.algorithm.name()} cannot be used for ${e.context}'
}
