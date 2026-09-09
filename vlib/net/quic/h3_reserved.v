module quic

// HTTP/3 (RFC 9114) reserves one identical "grease" codepoint pattern across
// FOUR independent numeric spaces -- frame types (§7.2.8), unidirectional
// stream types (§6.2.3), SETTINGS identifiers (§7.2.4.1), and error codes
// (§8.1) -- specifically so that a well-behaved implementation is forced to
// exercise its own "ignore what I don't understand" code path. All four
// citations use byte-for-byte identical wording ("0x1f * N + 0x21 for
// non-negative integer values of N ... 0x21, 0x40, ..., through
// 0x3ffffffffffffffe"), confirmed by reading each of §6.2.3/§7.2.4.1/
// §7.2.8/§8.1 directly rather than assuming they match. One shared helper
// avoids reimplementing (and risking a transcription slip in) the same
// formula four times.

// is_h3_reserved_codepoint reports whether v is one of the reserved "grease"
// values 0x1f*N+0x21 (N = 0, 1, 2, ...) shared by the frame type, stream
// type, SETTINGS identifier, and error code numeric spaces. These values
// MUST NOT be assigned meaning by any implementation and MUST NOT be
// treated as a protocol violation on receipt -- callers use this to route
// an otherwise-unrecognized codepoint to "ignore silently" rather than
// "reject as unknown/invalid".
pub fn is_h3_reserved_codepoint(v u64) bool {
	if v < 0x21 {
		return false
	}
	remainder := (v - 0x21) % 0x1f
	return remainder == 0
}
