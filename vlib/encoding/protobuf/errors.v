module protobuf

// Typed errors for decode failures. Pattern-match in callers:
//
//   protobuf.decode[User](data) or {
//       if err is protobuf.UnexpectedEofError { ... }
//   }
//
// Every error carries the byte offset it happened at, because a protobuf
// message has no line or column and the offset is the only way to locate a
// problem inside a nested payload.

// UnexpectedEofError reports a read that ran past the end of the available
// bytes. A declared length that overflows the host's `int` surfaces here too,
// with `need` clamped to what was actually left.
pub struct UnexpectedEofError {
	Error
pub:
	pos       int // offset at which the read began
	need      i64 // bytes the reader was trying to read
	remaining int // bytes actually available
}

// msg formats an UnexpectedEofError for `IError.msg()`.
pub fn (e &UnexpectedEofError) msg() string {
	return 'protobuf: unexpected end of input at pos ${e.pos}: need ${e.need} bytes, have ${e.remaining}'
}

// MalformedError reports input that is well-sized but not well-formed.
pub struct MalformedError {
	Error
pub:
	pos    int
	reason string
}

// msg formats a MalformedError for `IError.msg()`.
pub fn (e &MalformedError) msg() string {
	return 'protobuf: malformed at pos ${e.pos}: ${e.reason}'
}

// WireTypeMismatchError reports a known field arriving with a wire type its
// declared type cannot use. A repeated numeric field is not a mismatch when it
// arrives packed, so this covers only combinations the spec forbids.
pub struct WireTypeMismatchError {
	Error
pub:
	field    int
	expected WireType
	got      WireType
}

// msg formats a WireTypeMismatchError for `IError.msg()`.
pub fn (e &WireTypeMismatchError) msg() string {
	return 'protobuf: field ${e.field} cannot be read as wire type ${e.got}, expected ${e.expected}'
}

// UnknownWireTypeError reports a tag whose low three bits name a wire type the
// encoding spec never assigned, which makes the tag malformed rather than
// merely belonging to a field this build does not know.
pub struct UnknownWireTypeError {
	Error
pub:
	pos int
	got int
}

// msg formats an UnknownWireTypeError for `IError.msg()`.
pub fn (e &UnknownWireTypeError) msg() string {
	return 'protobuf: unknown wire type ${e.got} at pos ${e.pos}'
}

// MaxDepthError reports nested messages deeper than the reader will follow. A
// message that nests without bound is a denial-of-service, not a long message.
pub struct MaxDepthError {
	Error
pub:
	pos       int
	max_depth int
}

// msg formats a MaxDepthError for `IError.msg()`.
pub fn (e &MaxDepthError) msg() string {
	return 'protobuf: message nesting deeper than ${e.max_depth} at pos ${e.pos}'
}

// MaxLengthError reports a length-delimited payload, or a whole message, past
// the configured ceiling. The length on the wire is a 64-bit value, so this is
// the check that stops a small input from claiming a large allocation.
pub struct MaxLengthError {
	Error
pub:
	pos        int
	declared   i64
	max_length int
}

// msg formats a MaxLengthError for `IError.msg()`.
pub fn (e &MaxLengthError) msg() string {
	return 'protobuf: length ${e.declared} at pos ${e.pos} exceeds the ${e.max_length} byte limit'
}

// InvalidUtf8Error reports a `string` field whose bytes are not valid UTF-8.
// `bytes` fields are exempt, since they carry arbitrary octets by design.
pub struct InvalidUtf8Error {
	Error
pub:
	pos int
}

// msg formats an InvalidUtf8Error for `IError.msg()`.
pub fn (e &InvalidUtf8Error) msg() string {
	return 'protobuf: string field at pos ${e.pos} is not valid UTF-8'
}

// UnknownFieldError reports a field number this build has no mapping for, and
// only surfaces when the caller asked to reject them. By default such a field
// is skipped, because a newer producer must not break an older consumer.
pub struct UnknownFieldError {
	Error
pub:
	pos    int
	number int
}

// msg formats an UnknownFieldError for `IError.msg()`.
pub fn (e &UnknownFieldError) msg() string {
	return 'protobuf: unknown field number ${e.number} at pos ${e.pos}'
}

// MissingFieldError reports a field the schema requires that the input never
// carried.
pub struct MissingFieldError {
	Error
pub:
	name string
}

// msg formats a MissingFieldError for `IError.msg()`.
pub fn (e &MissingFieldError) msg() string {
	return 'protobuf: required field `${e.name}` is missing'
}

// GroupUnsupportedError reports a group, the deprecated proto2 construct that
// the encoding spec kept only for compatibility. Its extent cannot be found
// without reading fields the reader does not understand, so this module
// refuses it rather than guessing.
pub struct GroupUnsupportedError {
	Error
pub:
	pos    int
	number int
}

// msg formats a GroupUnsupportedError for `IError.msg()`.
pub fn (e &GroupUnsupportedError) msg() string {
	return 'protobuf: group field ${e.number} at pos ${e.pos} is not supported'
}

// unexpected_eof_at builds an UnexpectedEofError.
@[cold]
fn unexpected_eof_at(pos int, need int, remaining int) UnexpectedEofError {
	return UnexpectedEofError{
		pos:       pos
		need:      i64(need)
		remaining: remaining
	}
}

// malformed_at builds a MalformedError.
@[cold]
fn malformed_at(pos int, reason string) MalformedError {
	return MalformedError{
		pos:    pos
		reason: reason
	}
}

// wire_type_mismatch builds a WireTypeMismatchError.
@[cold]
fn wire_type_mismatch(field int, expected WireType, got WireType) WireTypeMismatchError {
	return WireTypeMismatchError{
		field:    field
		expected: expected
		got:      got
	}
}

// unknown_wire_type builds an UnknownWireTypeError.
@[cold]
fn unknown_wire_type(pos int, got int) UnknownWireTypeError {
	return UnknownWireTypeError{
		pos: pos
		got: got
	}
}

// max_depth_exceeded builds a MaxDepthError.
@[cold]
fn max_depth_exceeded(pos int, max_depth int) MaxDepthError {
	return MaxDepthError{
		pos:       pos
		max_depth: max_depth
	}
}

// max_length_exceeded builds a MaxLengthError.
@[cold]
fn max_length_exceeded(pos int, declared i64, max_length int) MaxLengthError {
	return MaxLengthError{
		pos:        pos
		declared:   declared
		max_length: max_length
	}
}

// invalid_utf8_at builds an InvalidUtf8Error.
@[cold]
fn invalid_utf8_at(pos int) InvalidUtf8Error {
	return InvalidUtf8Error{
		pos: pos
	}
}

// unknown_field_at builds an UnknownFieldError.
@[cold]
fn unknown_field_at(pos int, number int) UnknownFieldError {
	return UnknownFieldError{
		pos:    pos
		number: number
	}
}

// group_unsupported_at builds a GroupUnsupportedError.
@[cold]
fn group_unsupported_at(pos int, number int) GroupUnsupportedError {
	return GroupUnsupportedError{
		pos:    pos
		number: number
	}
}
