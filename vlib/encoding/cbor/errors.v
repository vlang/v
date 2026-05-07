module cbor

// Typed errors for CBOR decode failures. Pattern-match in callers:
//
//   cbor.decode[User](bad) or {
//       if err is cbor.UnexpectedEofError { ... }
//   }

// UnexpectedEofError fires when the decoder runs past the end of its input.
// `need` is i64 so it can represent the full CBOR length range (which is
// u64 on the wire); huge values are clamped to i64::max for reporting.
pub struct UnexpectedEofError {
	Error
pub:
	pos       int // position at which the read began
	need      i64 // bytes the decoder was trying to read
	remaining int // bytes actually available
}

// msg formats an UnexpectedEofError for `IError.msg()`.
pub fn (e &UnexpectedEofError) msg() string {
	if e.need == max_i64 {
		return 'cbor: unexpected EOF at pos ${e.pos}: declared length exceeds available input (have ${e.remaining})'
	}
	return 'cbor: unexpected EOF at pos ${e.pos}: need ${e.need} bytes, have ${e.remaining}'
}

// MalformedError fires when the byte stream violates RFC 8949 well-formedness.
pub struct MalformedError {
	Error
pub:
	pos    int
	reason string
}

// msg formats a MalformedError for `IError.msg()`.
pub fn (e &MalformedError) msg() string {
	return 'cbor: malformed at pos ${e.pos}: ${e.reason}'
}

// TypeMismatchError fires when a typed read finds a different major type.
pub struct TypeMismatchError {
	Error
pub:
	pos      int
	expected string
	got      u8 // initial byte
}

// msg formats a TypeMismatchError for `IError.msg()`.
pub fn (e &TypeMismatchError) msg() string {
	return 'cbor: type mismatch at pos ${e.pos}: expected ${e.expected}, got initial byte 0x${e.got:02x}'
}

// MaxDepthError fires when nested arrays/maps exceed the configured cap.
pub struct MaxDepthError {
	Error
pub:
	pos       int
	max_depth int
}

// msg formats a MaxDepthError for `IError.msg()`.
pub fn (e &MaxDepthError) msg() string {
	return 'cbor: max nesting depth ${e.max_depth} exceeded at pos ${e.pos}'
}

// UnknownFieldError fires when a struct decoded with `deny_unknown_fields`
// encounters an unmapped key.
pub struct UnknownFieldError {
	Error
pub:
	pos  int
	name string
}

// msg formats an UnknownFieldError for `IError.msg()`.
pub fn (e &UnknownFieldError) msg() string {
	return 'cbor: unknown field "${e.name}" at pos ${e.pos}'
}

// IntRangeError fires when a decoded integer doesn't fit the target type.
pub struct IntRangeError {
	Error
pub:
	pos    int
	target string
	value  string
}

// msg formats an IntRangeError for `IError.msg()`.
pub fn (e &IntRangeError) msg() string {
	return 'cbor: integer ${e.value} at pos ${e.pos} out of range for ${e.target}'
}

// InvalidUtf8Error fires when a text-string payload isn't valid UTF-8 and
// the decoder is configured to validate strings.
pub struct InvalidUtf8Error {
	Error
pub:
	pos int
}

// msg formats an InvalidUtf8Error for `IError.msg()`.
pub fn (e &InvalidUtf8Error) msg() string {
	return 'cbor: invalid UTF-8 in text string at pos ${e.pos}'
}

@[cold; inline]
fn eof_at(pos int) IError {
	return UnexpectedEofError{
		pos:       pos
		need:      1
		remaining: 0
	}
}

@[cold; inline]
fn eof_needing(pos int, need i64, remaining int) IError {
	return UnexpectedEofError{
		pos:       pos
		need:      need
		remaining: remaining
	}
}

// eof_oversized reports an EOF caused by a length argument larger than
// the host can represent, clamped to i64::max so callers see a sensible
// number rather than a negative wrap-around. Used by string/bytes
// chunk readers where the wire length is u64.
@[cold; inline]
fn eof_oversized(pos int, want u64, remaining int) IError {
	clamped := if want > u64(max_i64) { max_i64 } else { i64(want) }
	return UnexpectedEofError{
		pos:       pos
		need:      clamped
		remaining: remaining
	}
}

@[cold; inline]
fn malformed(pos int, reason string) IError {
	return MalformedError{
		pos:    pos
		reason: reason
	}
}

@[cold; inline]
fn type_mismatch(pos int, expected string, got u8) IError {
	return TypeMismatchError{
		pos:      pos
		expected: expected
		got:      got
	}
}

@[cold; inline]
fn int_range(pos int, target string, value string) IError {
	return IntRangeError{
		pos:    pos
		target: target
		value:  value
	}
}
