module cbor

import math

// DecodeOpts tunes the decoder. Defaults are conservative: UTF-8 is
// validated, depth is capped to fend off stack-blow-up payloads, and
// duplicate map keys are tolerated (callers that need detection turn
// `deny_duplicate_keys` on).
pub struct DecodeOpts {
pub:
	max_depth            int = 256
	max_stream_bytes     int // 0 = unbounded for stream readers
	validate_utf8        bool = true
	deny_unknown_fields  bool // struct decode rejects unmapped keys
	deny_duplicate_keys  bool // Map decode rejects repeated keys
	allow_trailing_bytes bool // accept extra bytes after the top-level item
}

// Kind classifies the next item without consuming it. Useful to branch
// before committing to a typed read.
pub enum Kind {
	unsigned   // major type 0
	negative   // major type 1
	bytes      // major type 2 (definite or indefinite)
	text       // major type 3 (definite or indefinite)
	array_val  // major type 4 (definite or indefinite)
	map_val    // major type 5 (definite or indefinite)
	tag_val    // major type 6
	bool_val   // simple 20/21
	null_val   // simple 22
	undefined  // simple 23
	simple_val // other simple values
	float_val  // half/single/double
	break_code // 0xff outside a definite header
}

// Unpacker walks a CBOR byte slice. Operates non-allocating where
// possible; strings and bytes returned by `unpack_text` / `unpack_bytes`
// always own their storage so they outlive the input buffer.
pub struct Unpacker {
pub mut:
	data []u8
	pos  int
	opts DecodeOpts
}

// new_unpacker constructs an Unpacker over the given byte slice.
pub fn new_unpacker(data []u8, opts DecodeOpts) Unpacker {
	cap := if opts.max_depth > 0 {
		opts
	} else {
		DecodeOpts{
			...opts
			max_depth: 256
		}
	}
	return Unpacker{
		data: data
		pos:  0
		opts: cap
	}
}

// remaining returns the number of unread bytes.
@[inline]
pub fn (u &Unpacker) remaining() int {
	return u.data.len - u.pos
}

// done reports whether the unpacker has consumed every byte.
@[inline]
pub fn (u &Unpacker) done() bool {
	return u.pos >= u.data.len
}

// --------------------------------------------------------------------
// Low-level byte reads
// --------------------------------------------------------------------

@[direct_array_access; inline]
fn (mut u Unpacker) read_byte() !u8 {
	if u.pos >= u.data.len {
		return eof_at(u.pos)
	}
	b := u.data[u.pos]
	u.pos++
	return b
}

@[direct_array_access; inline]
fn (u &Unpacker) peek_byte() !u8 {
	if u.pos >= u.data.len {
		return eof_at(u.pos)
	}
	return u.data[u.pos]
}

@[direct_array_access; inline]
fn (mut u Unpacker) read_be_u16() !u16 {
	if u.pos + 2 > u.data.len {
		return eof_needing(u.pos, 2, u.data.len - u.pos)
	}
	v := u16(u.data[u.pos]) << 8 | u16(u.data[u.pos + 1])
	u.pos += 2
	return v
}

@[direct_array_access; inline]
fn (mut u Unpacker) read_be_u32() !u32 {
	if u.pos + 4 > u.data.len {
		return eof_needing(u.pos, 4, u.data.len - u.pos)
	}
	v := u32(u.data[u.pos]) << 24 | u32(u.data[u.pos + 1]) << 16 | u32(u.data[u.pos + 2]) << 8 | u32(u.data[
		u.pos + 3])
	u.pos += 4
	return v
}

@[direct_array_access; inline]
fn (mut u Unpacker) read_be_u64() !u64 {
	if u.pos + 8 > u.data.len {
		return eof_needing(u.pos, 8, u.data.len - u.pos)
	}
	v := u64(u.data[u.pos]) << 56 | u64(u.data[u.pos + 1]) << 48 | u64(u.data[u.pos + 2]) << 40 | u64(u.data[
		u.pos + 3]) << 32 | u64(u.data[u.pos + 4]) << 24 | u64(u.data[u.pos + 5]) << 16 | u64(u.data[
		u.pos + 6]) << 8 | u64(u.data[u.pos + 7])
	u.pos += 8
	return v
}

// read_arg reads the additional-info argument for the given initial
// byte. Returns -1 to signal indefinite-length (info == 31) for major
// types that allow it; the caller decides whether that's legal.
fn (mut u Unpacker) read_arg(info u8) !u64 {
	match info {
		0...23 { return u64(info) }
		24 { return u64(u.read_byte()!) }
		25 { return u64(u.read_be_u16()!) }
		26 { return u64(u.read_be_u32()!) }
		27 { return u.read_be_u64()! }
		else { return malformed(u.pos - 1, 'reserved additional info ${info}') }
	}
}

// --------------------------------------------------------------------
// Public peek
// --------------------------------------------------------------------

// peek_kind classifies the next item without consuming any input.
pub fn (u &Unpacker) peek_kind() !Kind {
	if u.pos >= u.data.len {
		return eof_at(u.pos)
	}
	b := u.data[u.pos]
	major := b >> 5
	info := b & 0x1f
	match major {
		0 {
			return .unsigned
		}
		1 {
			return .negative
		}
		2 {
			return .bytes
		}
		3 {
			return .text
		}
		4 {
			return .array_val
		}
		5 {
			return .map_val
		}
		6 {
			return .tag_val
		}
		else {
			match info {
				20, 21 { return .bool_val }
				22 { return .null_val }
				23 { return .undefined }
				25, 26, 27 { return .float_val }
				31 { return .break_code }
				else { return .simple_val }
			}
		}
	}
}

// --------------------------------------------------------------------
// High-level typed reads
// --------------------------------------------------------------------

// unpack_uint reads a non-negative integer (major type 0). Errors on
// negatives, floats, or other major types. Position is rolled back on
// any error so callers can branch on `peek_kind` and try a different
// read (same convention as `unpack_bool` / `unpack_text`).
pub fn (mut u Unpacker) unpack_uint() !u64 {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	if major != 0 {
		u.pos = start
		return type_mismatch(start, 'unsigned', b)
	}
	return u.read_arg(b & 0x1f) or {
		u.pos = start
		return err
	}
}

// unpack_int reads any CBOR integer (major type 0 or 1) into i64. Errors
// when the magnitude exceeds i64 range; use `unpack_int_full` to pull
// values as u64 with a separate sign flag.
pub fn (mut u Unpacker) unpack_int() !i64 {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	arg := u.read_arg(b & 0x1f)!
	if major == 0 {
		if arg > u64(max_i64) {
			u.pos = start
			return int_range(start, 'i64', arg.str())
		}
		return i64(arg)
	}
	if major == 1 {
		// Represented integer = -1 - arg.
		if arg > u64(max_i64) {
			u.pos = start
			return int_range(start, 'i64', '-1 - ${arg}')
		}
		return -1 - i64(arg)
	}
	u.pos = start
	return type_mismatch(start, 'integer', b)
}

// unpack_int_full returns (negative, magnitude). For unsigned values
// negative=false and magnitude is the raw u64. For negative values
// negative=true and magnitude is the encoded argument (the integer
// itself is `-1 - magnitude`).
pub fn (mut u Unpacker) unpack_int_full() !(bool, u64) {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	arg := u.read_arg(b & 0x1f)!
	if major == 0 {
		return false, arg
	}
	if major == 1 {
		return true, arg
	}
	u.pos = start
	return type_mismatch(start, 'integer', b)
}

// unpack_bool reads a CBOR boolean (simple 20/21). Position is rolled
// back on a type mismatch so callers can branch on `peek_kind` and try
// a different read.
pub fn (mut u Unpacker) unpack_bool() !bool {
	start := u.pos
	b := u.read_byte()!
	if b == 0xf4 {
		return false
	}
	if b == 0xf5 {
		return true
	}
	u.pos = start
	return type_mismatch(start, 'bool', b)
}

// unpack_null consumes a CBOR null (0xf6) or errors with type mismatch.
// Position is rolled back on mismatch (same convention as unpack_bool).
pub fn (mut u Unpacker) unpack_null() ! {
	start := u.pos
	b := u.read_byte()!
	if b != 0xf6 {
		u.pos = start
		return type_mismatch(start, 'null', b)
	}
}

// unpack_float reads a CBOR float of any width (half/single/double) and
// returns it as f64.
pub fn (mut u Unpacker) unpack_float() !f64 {
	start := u.pos
	b := u.read_byte()!
	match b {
		0xf9 {
			h := u.read_be_u16()!
			return half_to_f64(h)
		}
		0xfa {
			bits := u.read_be_u32()!
			return f64(math.f32_from_bits(bits))
		}
		0xfb {
			bits := u.read_be_u64()!
			return math.f64_from_bits(bits)
		}
		else {
			u.pos = start
			return type_mismatch(start, 'float', b)
		}
	}
}

// unpack_simple reads a simple value (0..255). Bool/null/undefined are
// also simple values; this method returns the raw u8.
pub fn (mut u Unpacker) unpack_simple() !u8 {
	start := u.pos
	b := u.read_byte()!
	if b >= 0xe0 && b <= 0xf3 {
		return b & 0x1f
	}
	match b {
		0xf4 {
			return 20
		}
		0xf5 {
			return 21
		}
		0xf6 {
			return 22
		}
		0xf7 {
			return 23
		}
		0xf8 {
			v := u.read_byte()!
			if v < 32 {
				u.pos = start
				return malformed(start, 'simple value < 32 must use 1-byte form')
			}
			return v
		}
		else {
			u.pos = start
			return type_mismatch(start, 'simple', b)
		}
	}
}

// unpack_text reads a definite or indefinite-length text string. The
// returned string owns its bytes (it's a clone of the input slice).
// UTF-8 validation runs unless `DecodeOpts.validate_utf8` is false.
pub fn (mut u Unpacker) unpack_text() !string {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	if major != 3 {
		u.pos = start
		return type_mismatch(start, 'text', b)
	}
	info := b & 0x1f
	if info == 31 {
		return u.read_indef_text()!
	}
	size := u.read_arg(info)!
	return u.read_text_chunk(size)!
}

// read_text_chunk consumes `size` bytes as a UTF-8 text fragment. The
// argument is u64 because CBOR allows lengths up to 2^64-1 in the wire
// format; the function rejects any length that the host can't represent
// or that exceeds the available payload, so neither the bounds check
// nor the slice can panic on adversarial input.
@[direct_array_access]
fn (mut u Unpacker) read_text_chunk(size u64) !string {
	if size > u64(u.data.len - u.pos) {
		return eof_oversized(u.pos, size, u.data.len - u.pos)
	}
	size_int := int(size)
	bytes_start := u.pos
	u.pos += size_int
	if u.opts.validate_utf8 && !utf8_validate_slice(u.data, bytes_start, size_int) {
		return InvalidUtf8Error{
			pos: bytes_start
		}
	}
	return u.data[bytes_start..u.pos].bytestr()
}

fn (mut u Unpacker) read_indef_text() !string {
	mut acc := strings_builder_new()
	for {
		b := u.read_byte()!
		if b == 0xff {
			break
		}
		major := b >> 5
		info := b & 0x1f
		if major != 3 || info == 31 {
			return malformed(u.pos - 1,
				'indefinite-length text chunk must be a definite-length text string')
		}
		size := u.read_arg(info)!
		s := u.read_text_chunk(size)!
		acc.write_string(s)
	}
	return acc.str()
}

// unpack_bytes reads a definite or indefinite-length byte string. The
// returned slice is a clone, safe to retain after the unpacker is freed.
pub fn (mut u Unpacker) unpack_bytes() ![]u8 {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	if major != 2 {
		u.pos = start
		return type_mismatch(start, 'bytes', b)
	}
	info := b & 0x1f
	if info == 31 {
		return u.read_indef_bytes()!
	}
	size := u.read_arg(info)!
	return u.read_bytes_chunk(size)!
}

// read_bytes_chunk consumes `size` bytes as a byte string fragment.
// See read_text_chunk for why size is u64.
@[direct_array_access]
fn (mut u Unpacker) read_bytes_chunk(size u64) ![]u8 {
	if size > u64(u.data.len - u.pos) {
		return eof_oversized(u.pos, size, u.data.len - u.pos)
	}
	size_int := int(size)
	out := u.data[u.pos..u.pos + size_int].clone()
	u.pos += size_int
	return out
}

fn (mut u Unpacker) read_indef_bytes() ![]u8 {
	mut acc := []u8{cap: 64}
	for {
		b := u.read_byte()!
		if b == 0xff {
			break
		}
		major := b >> 5
		info := b & 0x1f
		if major != 2 || info == 31 {
			return malformed(u.pos - 1,
				'indefinite-length bytes chunk must be a definite-length byte string')
		}
		size := u.read_arg(info)!
		acc << u.read_bytes_chunk(size)!
	}
	return acc
}

// unpack_array_header reads the prefix of an array. Returns the count
// for definite-length arrays, or -1 for indefinite-length arrays (the
// caller then loops until peek_kind() == .break_code and consumes the
// break with `expect_break`).
pub fn (mut u Unpacker) unpack_array_header() !i64 {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	if major != 4 {
		u.pos = start
		return type_mismatch(start, 'array', b)
	}
	info := b & 0x1f
	if info == 31 {
		return -1
	}
	arg := u.read_arg(info)!
	if arg > u64(max_i64) {
		u.pos = start
		return int_range(start, 'i64', arg.str())
	}
	return i64(arg)
}

// unpack_map_header reads the prefix of a map. Returns pair count or -1
// for indefinite-length maps.
pub fn (mut u Unpacker) unpack_map_header() !i64 {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	if major != 5 {
		u.pos = start
		return type_mismatch(start, 'map', b)
	}
	info := b & 0x1f
	if info == 31 {
		return -1
	}
	arg := u.read_arg(info)!
	if arg > u64(max_i64) {
		u.pos = start
		return int_range(start, 'i64', arg.str())
	}
	return i64(arg)
}

// unpack_tag reads a tag header and returns the tag number. The caller
// must follow up by reading the tag content. Position is rolled back
// on any error so callers can branch on `peek_kind` and try a different
// read.
pub fn (mut u Unpacker) unpack_tag() !u64 {
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	if major != 6 {
		u.pos = start
		return type_mismatch(start, 'tag', b)
	}
	return u.read_arg(b & 0x1f) or {
		u.pos = start
		return err
	}
}

// peek_break reports whether the next byte is the break stop code.
@[inline]
pub fn (u &Unpacker) peek_break() bool {
	return u.pos < u.data.len && u.data[u.pos] == 0xff
}

// consume_break advances past a break stop code if one is at the
// cursor, returning true. Useful for the indef-length loop pattern:
// `for { if u.consume_break() { break } ... }`.
@[inline]
fn (mut u Unpacker) consume_break() bool {
	if u.peek_break() {
		u.pos++
		return true
	}
	return false
}

// check_container_len rejects a definite-length array or map header
// whose item count can't fit a host `int` or whose minimum byte cost
// (1 byte/item for arrays, 2 bytes/pair for maps) already exceeds the
// remaining payload. Callers use the `int(n)` cast safely after.
//
// Comparison uses `remaining / bytes_per_item` rather than
// `n * bytes_per_item` so the multiplication can't overflow at
// n ≈ i64::max.
@[inline]
fn (u &Unpacker) check_container_len(start int, n u64, bytes_per_item int, kind string) ! {
	if n > u64(max_i64) || i64(n) > i64(u.data.len - u.pos) / i64(bytes_per_item) {
		return malformed(start, '${kind} length ${n} exceeds remaining input')
	}
}

// expect_break consumes a single 0xff break code; errors otherwise.
pub fn (mut u Unpacker) expect_break() ! {
	b := u.read_byte()!
	if b != 0xff {
		return malformed(u.pos - 1, 'expected break code, got 0x${b:02x}')
	}
}

// --------------------------------------------------------------------
// Skip
// --------------------------------------------------------------------

// skip_value advances past one complete CBOR value without allocating.
// Honours the depth cap so adversarial deeply-nested input cannot blow
// the stack.
pub fn (mut u Unpacker) skip_value() ! {
	u.skip_inner(0)!
}

fn (mut u Unpacker) skip_inner(depth int) ! {
	if depth > u.opts.max_depth {
		return MaxDepthError{
			pos:       u.pos
			max_depth: u.opts.max_depth
		}
	}
	b := u.read_byte()!
	major := b >> 5
	info := b & 0x1f
	match major {
		0, 1 {
			u.read_arg(info)!
		}
		2, 3 {
			if info == 31 {
				// RFC 8949 §3.2.3: each chunk MUST be a definite-length
				// string of the same major type — no nested indefinite,
				// no cross-type chunks. Mirror unpack_text/unpack_bytes.
				for {
					if u.consume_break() {
						break
					}
					cb := u.read_byte()!
					cmajor := cb >> 5
					cinfo := cb & 0x1f
					if cmajor != major || cinfo == 31 {
						return malformed(u.pos - 1,
							'indefinite-length string chunk must be a definite-length string of the same major type')
					}
					csize := u.read_arg(cinfo)!
					if csize > u64(u.data.len - u.pos) {
						return eof_oversized(u.pos, csize, u.data.len - u.pos)
					}
					u.pos += int(csize)
				}
			} else {
				size := u.read_arg(info)!
				if size > u64(u.data.len - u.pos) {
					return eof_oversized(u.pos, size, u.data.len - u.pos)
				}
				u.pos += int(size)
			}
		}
		4 {
			if info == 31 {
				for {
					if u.consume_break() {
						break
					}
					u.skip_inner(depth + 1)!
				}
			} else {
				n := u.read_arg(info)!
				u.check_container_len(u.pos - 1, n, 1, 'array')!
				for _ in 0 .. n {
					u.skip_inner(depth + 1)!
				}
			}
		}
		5 {
			if info == 31 {
				for {
					if u.consume_break() {
						break
					}
					u.skip_inner(depth + 1)! // key
					u.skip_inner(depth + 1)! // value
				}
			} else {
				n := u.read_arg(info)!
				u.check_container_len(u.pos - 1, n, 2, 'map')!
				for _ in 0 .. n {
					u.skip_inner(depth + 1)!
					u.skip_inner(depth + 1)!
				}
			}
		}
		6 {
			u.read_arg(info)!
			u.skip_inner(depth + 1)!
		}
		else {
			// Major type 7 (floats / simple).
			match info {
				0...23 {} // simple values 0..23 inline
				24 {
					// Per RFC 8949 §3.3, a simple value < 32 must use the
					// inline form (info 0..23) — the 1-byte form is only
					// well-formed for 32..255. unpack_simple already enforces
					// this; skip_value must too, otherwise malformed CBOR
					// slips through RawMessage / Unmarshaler / unknown-field
					// skipping and lands in downstream consumers.
					sv := u.read_byte()!
					if sv < 32 {
						return malformed(u.pos - 1, 'simple value < 32 must use 1-byte form')
					}
				}
				25 {
					u.pos += 2
				} // half
				26 {
					u.pos += 4
				} // single
				27 {
					u.pos += 8
				} // double
				31 {
					return malformed(u.pos - 1, 'unexpected break stop code')
				}
				else {
					return malformed(u.pos - 1, 'reserved additional info ${info}')
				}
			}

			if u.pos > u.data.len {
				return eof_at(u.data.len)
			}
		}
	}
}

// --------------------------------------------------------------------
// Value tree decoder
// --------------------------------------------------------------------

// unpack_value materialises one CBOR data item as a Value.
pub fn (mut u Unpacker) unpack_value() !Value {
	return u.unpack_value_inner(0)!
}

fn (mut u Unpacker) unpack_value_inner(depth int) !Value {
	if depth > u.opts.max_depth {
		return MaxDepthError{
			pos:       u.pos
			max_depth: u.opts.max_depth
		}
	}
	start := u.pos
	b := u.read_byte()!
	major := b >> 5
	info := b & 0x1f
	match major {
		0 {
			arg := u.read_arg(info)!
			return IntNum{
				negative:  false
				magnitude: arg
			}
		}
		1 {
			arg := u.read_arg(info)!
			return IntNum{
				negative:  true
				magnitude: arg
			}
		}
		2 {
			u.pos = start
			data := u.unpack_bytes()!
			return Bytes{
				data: data
			}
		}
		3 {
			u.pos = start
			s := u.unpack_text()!
			return Text{
				value: s
			}
		}
		4 {
			if info == 31 {
				mut elements := []Value{cap: 4}
				for {
					if u.consume_break() {
						break
					}
					elements << u.unpack_value_inner(depth + 1)!
				}
				return Array{
					elements: elements
				}
			}
			n := u.read_arg(info)!
			u.check_container_len(start, n, 1, 'array')!
			mut elements := []Value{cap: int(n)}
			for _ in 0 .. n {
				elements << u.unpack_value_inner(depth + 1)!
			}
			return Array{
				elements: elements
			}
		}
		5 {
			// Dedup tracking hashes the raw on-wire bytes of each key (per
			// RFC 8949 §5.6 "encoded data items are equal iff their byte
			// representations match") in a V map → O(1) lookup vs the
			// previous O(n) linear scan, so adversarial inputs with
			// thousands of distinct keys decode in linear time. Built
			// only when the option is set.
			mut seen := map[string]bool{}
			if info == 31 {
				mut pairs := []MapPair{cap: 4}
				for {
					if u.consume_break() {
						break
					}
					key_start := u.pos
					key := u.unpack_value_inner(depth + 1)!
					if u.opts.deny_duplicate_keys {
						k := u.data[key_start..u.pos].bytestr()
						if k in seen {
							return malformed(key_start, 'duplicate map key')
						}
						seen[k] = true
					}
					val := u.unpack_value_inner(depth + 1)!
					pairs << MapPair{
						key:   key
						value: val
					}
				}
				return Map{
					pairs: pairs
				}
			}
			n := u.read_arg(info)!
			u.check_container_len(start, n, 2, 'map')!
			mut pairs := []MapPair{cap: int(n)}
			for _ in 0 .. n {
				key_start := u.pos
				key := u.unpack_value_inner(depth + 1)!
				if u.opts.deny_duplicate_keys {
					k := u.data[key_start..u.pos].bytestr()
					if k in seen {
						return malformed(key_start, 'duplicate map key')
					}
					seen[k] = true
				}
				val := u.unpack_value_inner(depth + 1)!
				pairs << MapPair{
					key:   key
					value: val
				}
			}
			return Map{
				pairs: pairs
			}
		}
		6 {
			number := u.read_arg(info)!
			content := u.unpack_value_inner(depth + 1)!
			// Native validation per RFC 8949 §3.4.1: tag 0 wraps an RFC 3339
			// text string; tag 1 wraps a numeric value (int or float).
			// QCBOR does the same — accepting wrong content types here would
			// allow well-formed-but-invalid payloads through.
			if number == 0 && content !is Text {
				return malformed(u.pos, 'tag 0 (date/time) must wrap a text string')
			}
			if number == 1 && content !is IntNum && content !is FloatNum {
				return malformed(u.pos, 'tag 1 (epoch) must wrap a number')
			}
			return Tag{
				number:      number
				content_box: [content]
			}
		}
		else {
			match info {
				20 {
					return Bool{
						value: false
					}
				}
				21 {
					return Bool{
						value: true
					}
				}
				22 {
					return Null{}
				}
				23 {
					return Undefined{}
				}
				24 {
					v := u.read_byte()!
					if v < 32 {
						u.pos = start
						return malformed(start, 'simple value < 32 must use 1-byte form')
					}
					return Simple{
						value: v
					}
				}
				25 {
					h := u.read_be_u16()!
					return FloatNum{
						value: half_to_f64(h)
						bits:  .half
					}
				}
				26 {
					bits := u.read_be_u32()!
					return FloatNum{
						value: f64(math.f32_from_bits(bits))
						bits:  .single
					}
				}
				27 {
					bits := u.read_be_u64()!
					return FloatNum{
						value: math.f64_from_bits(bits)
						bits:  .double
					}
				}
				31 {
					u.pos = start
					return malformed(start, 'unexpected break stop code')
				}
				else {
					if info <= 19 {
						return Simple{
							value: info
						}
					}
					u.pos = start
					return malformed(start, 'reserved additional info ${info}')
				}
			}
		}
	}
}

// strings_builder_new is a small alias to keep the import surface tight
// (we only need the strings module for indefinite-length text accumulation).
@[inline]
fn strings_builder_new() StringsBuilder {
	return StringsBuilder{
		buf: []u8{cap: 32}
	}
}

struct StringsBuilder {
mut:
	buf []u8
}

@[inline]
fn (mut b StringsBuilder) write_string(s string) {
	if s == '' {
		return
	}
	unsafe { b.buf.push_many(s.str, s.len) }
}

@[inline]
fn (mut b StringsBuilder) str() string {
	return b.buf.bytestr()
}
