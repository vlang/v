// Tests that the decoder is safe against adversarial input: malformed
// initial bytes, premature EOF, depth bombs, indefinite-length nesting,
// and invalid UTF-8.
module main

import encoding.cbor
import encoding.hex

fn h(s string) []u8 {
	return hex.decode(s) or { panic('invalid hex: ${s}') }
}

// ---------------------------------------------------------------------
// EOF handling
// ---------------------------------------------------------------------

fn test_eof_truncated_uint() {
	// 0x18 = uint(1-byte arg), but no following byte.
	if _ := cbor.decode[u64](h('18'), cbor.DecodeOpts{}) {
		assert false, 'expected EOF error'
	}
}

fn test_eof_truncated_array() {
	// 0x83 = array of 3, but only 1 element follows.
	if _ := cbor.decode[[]int](h('8301'), cbor.DecodeOpts{}) {
		assert false, 'expected EOF error in array'
	}
}

fn test_eof_truncated_string() {
	// 0x65 = text len 5, but only 3 bytes follow.
	if _ := cbor.decode[string](h('656162'), cbor.DecodeOpts{}) {
		assert false, 'expected EOF error in text'
	}
}

// ---------------------------------------------------------------------
// Reserved additional info
// ---------------------------------------------------------------------

fn test_reserved_info_rejected() {
	// 0x1c = major 0, info 28 (reserved).
	if _ := cbor.decode[cbor.Value](h('1c'), cbor.DecodeOpts{}) {
		assert false, 'expected malformed for info 28'
	}
}

// ---------------------------------------------------------------------
// Depth bomb
// ---------------------------------------------------------------------

fn test_depth_bomb_rejected() {
	// Build an indefinite-length array nested 1000 deep.
	mut deep := []u8{cap: 2002}
	for _ in 0 .. 1000 {
		deep << 0x9f // start indefinite array
	}
	for _ in 0 .. 1000 {
		deep << 0xff // close
	}
	if _ := cbor.decode[cbor.Value](deep, cbor.DecodeOpts{ max_depth: 16 }) {
		assert false, 'expected MaxDepthError'
	}
}

// ---------------------------------------------------------------------
// Indefinite-length string with mismatched chunk
// ---------------------------------------------------------------------

fn test_indef_text_with_byte_chunk_rejected() {
	// 0x7f = indef text. 0x42 = bytes(2). Should fail.
	if _ := cbor.decode[string](h('7f4201020203ff'), cbor.DecodeOpts{}) {
		assert false, 'expected malformed for mixed indef-text chunk'
	}
}

fn test_nested_indef_text_rejected() {
	// 0x7f7f...ff is indef text containing indef text — disallowed.
	if _ := cbor.decode[string](h('7f7f60ffff'), cbor.DecodeOpts{}) {
		assert false, 'expected malformed for nested indef text'
	}
}

// ---------------------------------------------------------------------
// UTF-8 validation
// ---------------------------------------------------------------------

fn test_invalid_utf8_rejected() {
	// 0x62 = text len 2, then invalid 2-byte sequence 0xc3 0x28.
	if _ := cbor.decode[string](h('62c328'), cbor.DecodeOpts{}) {
		assert false, 'expected InvalidUtf8Error'
	}
}

fn test_invalid_utf8_can_be_disabled() {
	// Same input but with validate_utf8 = false succeeds (caller
	// accepts responsibility for handling raw bytes).
	got := cbor.decode[string](h('62c328'), cbor.DecodeOpts{ validate_utf8: false }) or {
		panic('expected success: ${err}')
	}
	assert got.len == 2
}

fn test_invalid_utf8_overlong_rejected() {
	// "/" = 0x2f, but encoded as 2-byte overlong 0xc0 0xaf — rejected.
	if _ := cbor.decode[string](h('62c0af'), cbor.DecodeOpts{}) {
		assert false, 'expected InvalidUtf8Error for overlong'
	}
}

fn test_invalid_utf8_surrogate_rejected() {
	// U+D800 (high surrogate) in 3-byte form: 0xed 0xa0 0x80.
	if _ := cbor.decode[string](h('63eda080'), cbor.DecodeOpts{}) {
		assert false, 'expected InvalidUtf8Error for surrogate'
	}
}

// ---------------------------------------------------------------------
// Unknown fields in struct decode
// ---------------------------------------------------------------------

struct Strict {
	a int
}

fn test_unknown_field_tolerated_by_default() {
	// {"a": 1, "b": 2}
	bytes := h('a26161016162 02'.replace(' ', ''))
	got := cbor.decode[Strict](bytes, cbor.DecodeOpts{})!
	assert got.a == 1
}

fn test_unknown_field_rejected_when_opted_in() {
	bytes := h('a26161016162 02'.replace(' ', ''))
	if _ := cbor.decode[Strict](bytes, cbor.DecodeOpts{ deny_unknown_fields: true }) {
		assert false, 'expected UnknownFieldError'
	}
}

// ---------------------------------------------------------------------
// Native tag 0/1 content-type validation (RFC 8949 §3.4.1). Unlike a
// permissive decoder, we reject tag 0 wrapping non-text and tag 1
// wrapping non-numbers — same behaviour as QCBOR (the IETF reference).
// These cases come from the cbor-wg/bad conformance corpus.
// ---------------------------------------------------------------------

fn test_tag0_wrapping_map_rejected() {
	// c0 a1 61 61 00 = tag(0, {"a": 0}) — tag 0 must be tstr.
	if _ := cbor.decode[cbor.Value](h('c0a1616100'), cbor.DecodeOpts{}) {
		assert false, 'expected tag-0 type rejection'
	}
}

fn test_tag1_wrapping_map_rejected() {
	// c1 a1 61 61 00 = tag(1, {"a": 0}) — tag 1 must be int or float.
	if _ := cbor.decode[cbor.Value](h('c1a1616100'), cbor.DecodeOpts{}) {
		assert false, 'expected tag-1 type rejection'
	}
}

fn test_tag0_wrapping_text_accepted() {
	// c0 74 ... = tag(0, "2013-03-21T20:04:00Z") — well-formed.
	v := cbor.decode[cbor.Value](h('c074323031332d30332d32315432303a30343a30305a'), cbor.DecodeOpts{}) or {
		assert false, 'tag 0 + text MUST be accepted: ${err}'
		return
	}
	assert v is cbor.Tag
}

fn test_tag1_wrapping_int_or_float_accepted() {
	v := cbor.decode[cbor.Value](h('c11a514b67b0'), cbor.DecodeOpts{}) or {
		assert false, 'tag 1 + int MUST be accepted: ${err}'
		return
	}
	assert v is cbor.Tag

	v2 := cbor.decode[cbor.Value](h('c1fb41d452d9ec200000'), cbor.DecodeOpts{}) or {
		assert false, 'tag 1 + float MUST be accepted: ${err}'
		return
	}
	assert v2 is cbor.Tag
}

// ---------------------------------------------------------------------
// Header-length overflow: lengths beyond i64::max must be rejected,
// not silently wrapped to -1 (which would alias the indefinite-length
// sentinel and steer callers into the wrong loop). See decoder.v
// `unpack_array_header` / `unpack_map_header`.
// ---------------------------------------------------------------------

fn test_array_header_oversized_length_rejected() {
	// 9b ff ff ff ff ff ff ff ff = array, info=27, arg=u64::max.
	mut u := cbor.new_unpacker(h('9bffffffffffffffff'), cbor.DecodeOpts{})
	if n := u.unpack_array_header() {
		assert false, 'expected oversized length rejection, got ${n}'
	}
}

fn test_map_header_oversized_length_rejected() {
	// bb ff ff ff ff ff ff ff ff = map, info=27, arg=u64::max.
	mut u := cbor.new_unpacker(h('bbffffffffffffffff'), cbor.DecodeOpts{})
	if n := u.unpack_map_header() {
		assert false, 'expected oversized length rejection, got ${n}'
	}
}

fn test_array_header_at_i64_max_accepted() {
	// 9b 7f ff ff ff ff ff ff ff = array, info=27, arg=i64::max — boundary.
	mut u := cbor.new_unpacker(h('9b7fffffffffffffff'), cbor.DecodeOpts{})
	n := u.unpack_array_header() or {
		assert false, 'i64::max boundary must succeed: ${err}'
		return
	}
	assert n == max_i64
}

// ---------------------------------------------------------------------
// skip_value MUST enforce RFC 8949 §3.2.3 chunk rules for indefinite
// strings: each chunk must be a definite-length string of the same
// major type. Otherwise the skip path silently accepts what
// unpack_text / unpack_bytes correctly reject — letting malformed
// CBOR through RawMessage / Unmarshaler / unknown-field skipping.
// ---------------------------------------------------------------------

fn test_skip_value_rejects_cross_type_indef_string_chunk() {
	// 7f 41 00 ff = indef text containing one bytes chunk (major=2),
	// then break. Same chunk-type rule applies to skip_value as to
	// unpack_text — both reject cross-type chunks.
	mut u := cbor.new_unpacker(h('7f4100ff'), cbor.DecodeOpts{})
	if _ := u.skip_value() {
		assert false, 'skip_value must reject cross-type indef chunk'
	}
}

fn test_skip_value_rejects_nested_indef_string_chunk() {
	// 7f 7f 61 61 ff ff = indef text whose chunk is itself indefinite.
	mut u := cbor.new_unpacker(h('7f7f6161ffff'), cbor.DecodeOpts{})
	if _ := u.skip_value() {
		assert false, 'skip_value must reject nested indef chunk'
	}
}

fn test_raw_message_rejects_malformed_indef_string() {
	// Same payload as above, but exercised through the RawMessage path
	// (which calls skip_value internally to compute the slice bounds).
	mut u := cbor.new_unpacker(h('7f4100ff'), cbor.DecodeOpts{})
	if _ := u.unpack_raw() {
		assert false, 'unpack_raw must reject cross-type indef chunk'
	}
}
