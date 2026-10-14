// Coverage for the parts of the cbor API that the rest of the suite does not
// touch: the `Value` sumtype accessors and constructors, the decoder's
// type-dispatched reads, and the Packer's float widths and buffer controls.
//
// Everything here is deterministic and pure -- no clocks, no filesystem, no
// network. Assertions are on measured behaviour, not on what the RFC suggests
// the behaviour ought to be.
module main

import encoding.cbor
import encoding.hex
import math

fn b(s string) []u8 {
	return hex.decode(s) or { panic('invalid hex: ${s}') }
}

fn hx(bs []u8) string {
	return bs.hex()
}

// ---------------------------------------------------------------------
// Value: constructors and the integer accessors
// ---------------------------------------------------------------------

fn test_new_int_covers_both_signs() {
	assert cbor.new_int(0).as_int() or { panic('new_int(0) must be readable') } == 0
	assert cbor.new_int(1).as_int() or { panic('new_int(1)') } == 1
	assert cbor.new_int(-1).as_int() or { panic('new_int(-1)') } == -1
	assert cbor.new_int(i64(-9223372036854775808)).as_int() or { panic('i64::min') } == i64(-9223372036854775808)
	assert cbor.new_int(i64(9223372036854775807)).as_int() or { panic('i64::max') } == i64(9223372036854775807)
}

fn test_new_negative_uses_the_encoded_argument() {
	// new_negative(x) means the integer -1 - x, so 0 is -1.
	assert cbor.new_negative(0).as_int() or { panic('new_negative(0)') } == -1
	assert cbor.new_negative(1).as_int() or { panic('new_negative(1)') } == -2
	// i64::min is representable as magnitude i64::max, since -1 - max is min.
	assert cbor.new_negative(u64(9223372036854775807)).as_int() or { panic('magnitude i64::max') } == i64(-9223372036854775808)
}

fn test_as_int_returns_none_outside_i64_range() {
	// 2^63 does not fit i64 as an unsigned value.
	if _ := cbor.new_uint(u64(9223372036854775808)).as_int() {
		assert false, 'unsigned magnitude above i64::max must not be coerced'
	}
	// -1 - (2^64-1) is far below i64::min.
	if _ := cbor.new_negative(u64(18446744073709551615)).as_int() {
		assert false, 'negative magnitude u64::max must not be coerced'
	}
}

fn test_as_uint_rejects_negative_values() {
	if _ := cbor.new_int(-1).as_uint() {
		assert false, 'a negative integer has no unsigned reading'
	}
	if _ := cbor.new_negative(u64(18446744073709551615)).as_uint() {
		assert false, 'new_negative is negative by construction'
	}
	assert cbor.new_uint(u64(18446744073709551615)).as_uint() or { panic('u64::max') } == u64(18446744073709551615)
}

fn test_new_text_new_bytes_and_new_tag_constructors() {
	assert cbor.new_text('abc').as_string() or { panic('text') } == 'abc'
	assert cbor.new_bytes([u8(1), 2, 3]).as_bytes() or { panic('bytes') } == [u8(1), 2, 3]
	number, content := cbor.new_tag(3, cbor.new_int(7)).as_tag() or { panic('tag') }
	assert number == u64(3)
	assert content.as_int() or { panic('tag content') } == 7
}

// ---------------------------------------------------------------------
// Value: is_nil / is_undefined
// ---------------------------------------------------------------------

fn test_is_nil_and_is_undefined_only_match_their_own_variant() {
	assert cbor.Value(cbor.Null{}).is_nil()
	assert cbor.Value(cbor.Null{}).is_undefined() == false
	assert cbor.Value(cbor.Undefined{}).is_undefined()
	assert cbor.Value(cbor.Undefined{}).is_nil() == false
	assert cbor.new_int(0).is_nil() == false
	assert cbor.new_text('null').is_undefined() == false
}

// ---------------------------------------------------------------------
// Value: len
// ---------------------------------------------------------------------

fn test_len_counts_the_content_of_container_and_string_variants() {
	arr := cbor.Value(cbor.Array{
		elements: [cbor.new_int(1), cbor.new_int(2), cbor.new_int(3)]
	})
	assert arr.len() == 3
	m := cbor.Value(cbor.Map{
		pairs: [
			cbor.MapPair{
				key:   cbor.new_text('a')
				value: cbor.new_int(1)
			},
			cbor.MapPair{
				key:   cbor.new_text('b')
				value: cbor.new_int(2)
			},
		]
	})
	assert m.len() == 2
	assert cbor.new_text('abcd').len() == 4
	assert cbor.new_bytes([u8(1), 2]).len() == 2
}

fn test_len_is_zero_for_scalar_variants() {
	assert cbor.new_int(42).len() == 0
	assert cbor.new_float(2.5).len() == 0
	assert cbor.Value(cbor.Null{}).len() == 0
	assert cbor.Value(cbor.Undefined{}).len() == 0
	assert cbor.Value(cbor.Bool{ value: true }).len() == 0
}

// ---------------------------------------------------------------------
// Value: at and get
// ---------------------------------------------------------------------

fn test_at_indexes_arrays_and_reports_misses() {
	arr := cbor.Value(cbor.Array{
		elements: [cbor.new_int(10), cbor.new_int(20), cbor.new_int(30)]
	})
	assert arr.at(0) or { panic('at(0)') }.as_int() or { panic('at(0) int') } == 10
	assert arr.at(2) or { panic('at(2)') }.as_int() or { panic('at(2) int') } == 30
	if _ := arr.at(3) {
		assert false, 'index == len is out of range'
	}
	if _ := arr.at(-1) {
		assert false, 'a negative index is out of range'
	}
	if _ := cbor.new_text('not an array').at(0) {
		assert false, 'at must not read from a Text value'
	}
}

fn test_get_looks_up_string_keys_only() {
	m := cbor.Value(cbor.Map{
		pairs: [
			cbor.MapPair{
				key:   cbor.new_text('a')
				value: cbor.new_int(1)
			},
			cbor.MapPair{
				key:   cbor.new_text('b')
				value: cbor.new_text('two')
			},
			cbor.MapPair{
				key:   cbor.new_int(9)
				value: cbor.new_int(3)
			},
		]
	})
	assert m.get('a') or { panic('get(a)') }.as_int() or { panic('get(a) int') } == 1
	assert m.get('b') or { panic('get(b)') }.as_string() or { panic('get(b) text') } == 'two'
	if _ := m.get('missing') {
		assert false, 'an absent key must read as none'
	}
	// A non-Text key is never compared against a string, even when the
	// integer would format to it.
	if _ := m.get('9') {
		assert false, 'an integer key must not match the string "9"'
	}
	if _ := cbor.new_int(1).get('a') {
		assert false, 'get must not read from a non-Map value'
	}
}

fn test_as_array_and_as_map_return_none_on_the_wrong_variant() {
	arr := cbor.Value(cbor.Array{
		elements: [cbor.new_int(1)]
	})
	m := cbor.Value(cbor.Map{
		pairs: [cbor.MapPair{
			key:   cbor.new_text('k')
			value: cbor.new_int(1)
		}]
	})
	assert arr.as_array() or { panic('as_array') }.len == 1
	assert m.as_map() or { panic('as_map') }.len == 1
	if _ := cbor.new_text('x').as_array() {
		assert false, 'a Text value is not an Array'
	}
	if _ := cbor.new_int(1).as_map() {
		assert false, 'an IntNum value is not a Map'
	}
}

// ---------------------------------------------------------------------
// Value: as_bool / as_float / as_string / as_bytes / as_tag
// ---------------------------------------------------------------------

fn test_as_bool_as_float_as_string_as_bytes_match_only_their_variant() {
	bv := cbor.Value(cbor.Bool{
		value: true
	})
	assert bv.as_bool() or { panic('as_bool') }
	fv := cbor.Value(cbor.Bool{
		value: false
	})
	assert fv.as_bool() or { panic('as_bool false') } == false
	assert cbor.new_float(2.5).as_float() or { panic('as_float') } == 2.5
	assert cbor.new_text('hi').as_string() or { panic('as_string') } == 'hi'
	assert cbor.new_bytes([u8(0xde), 0xad]).as_bytes() or { panic('as_bytes') } == [
		u8(0xde),
		0xad,
	]

	if _ := cbor.new_int(1).as_bool() {
		assert false, 'an integer is not a CBOR bool'
	}
	if _ := cbor.new_int(1).as_float() {
		assert false, 'an integer is not a CBOR float'
	}
	if _ := cbor.new_bytes([u8(1)]).as_string() {
		assert false, 'a byte string is not a text string'
	}
	if _ := cbor.new_text('a').as_bytes() {
		assert false, 'a text string is not a byte string'
	}
}

fn test_as_tag_reads_number_and_content_and_none_elsewhere() {
	number, content := cbor.new_tag(1, cbor.new_int(5)).as_tag() or { panic('as_tag') }
	assert number == u64(1)
	assert content.as_int() or { panic('tag content') } == 5
	// A Tag with no stored content reads as Null rather than panicking.
	empty := cbor.Value(cbor.Tag{
		number: u64(7)
	})
	enum2, empty_content := empty.as_tag() or { panic('empty as_tag') }
	assert enum2 == u64(7)
	assert empty_content.is_nil()
	if _, _ := cbor.new_int(1).as_tag() {
		assert false, 'a non-Tag value must read as none'
	}
}

fn test_tag_content_direct_accessor_matches_as_tag() {
	// The doc-comment path: read the stored box directly rather than through
	// the sumtype accessor.
	t := cbor.Tag{
		number:      u64(4)
		content_box: [cbor.new_bytes([u8(7)])]
	}
	assert t.content().as_bytes() or { panic('Tag.content') } == [u8(7)]
	// An empty box reads as Null rather than panicking.
	empty := cbor.Tag{
		number: u64(9)
	}
	assert empty.content().is_nil()

	tagged := cbor.new_tag(4, cbor.new_bytes([u8(7)]))
	_, content := tagged.as_tag() or { panic('as_tag') }
	assert content.as_bytes() or { panic('tag bytes') } == [u8(7)]
}

// ---------------------------------------------------------------------
// Value: encode/decode round trips
// ---------------------------------------------------------------------

fn test_value_round_trip_preserves_the_data_item() {
	values := [
		cbor.new_int(0),
		cbor.new_int(42),
		cbor.new_int(-42),
		cbor.new_text('hello'),
		cbor.new_bytes([u8(1), 2, 3]),
		cbor.Value(cbor.Bool{ value: true }),
		cbor.Value(cbor.Bool{ value: false }),
		cbor.Value(cbor.Null{}),
		cbor.Value(cbor.Undefined{}),
		cbor.new_tag(1, cbor.new_int(5)),
		cbor.Value(cbor.Array{
			elements: [cbor.new_int(1), cbor.new_text('two'), cbor.Value(cbor.Bool{ value: false })]
		}),
	]
	for v in values {
		enc := cbor.encode[cbor.Value](v, cbor.EncodeOpts{}) or { panic('encode') }
		back := cbor.decode[cbor.Value](enc, cbor.DecodeOpts{}) or { panic('decode') }
		// NOTE: `new_float` sets bits=.@none while a decoded float records the
		// width it arrived at, so floats are compared by value below rather
		// than with `==` on the whole sumtype.
		if x := v.as_float() {
			assert back.as_float() or { panic('float round trip') } == x
			continue
		}
		assert back == v, '${v.type_name()} did not survive a round trip'
	}
}

// ---------------------------------------------------------------------
// Unpacker: position bookkeeping and peek_kind
// ---------------------------------------------------------------------

fn test_remaining_and_done_track_the_cursor() {
	mut u := cbor.new_unpacker(b('0102'), cbor.DecodeOpts{})
	assert u.remaining() == 2
	assert u.done() == false
	u.unpack_uint() or { panic('first uint') }
	assert u.remaining() == 1
	u.unpack_uint() or { panic('second uint') }
	assert u.remaining() == 0
	assert u.done()
}

struct KindCase {
	hexs string
	kind cbor.Kind
}

fn test_peek_kind_classifies_every_major_type_without_consuming() {
	cases := [
		KindCase{'00', .unsigned}, // major 0, immediate
		KindCase{'17', .unsigned},
		KindCase{'18ff', .unsigned}, // major 0, 1-byte argument follows
		KindCase{'20', .negative}, // major 1
		KindCase{'37', .negative},
		KindCase{'40', .bytes}, // major 2, definite
		KindCase{'5f', .bytes}, // major 2, indefinite
		KindCase{'60', .text}, // major 3, definite
		KindCase{'7f', .text}, // major 3, indefinite
		KindCase{'80', .array_val},
		KindCase{'9f', .array_val},
		KindCase{'a0', .map_val},
		KindCase{'bf', .map_val},
		KindCase{'c0', .tag_val},
		KindCase{'c6', .tag_val}, // tag 6
		KindCase{'d7', .tag_val},
		KindCase{'e0', .simple_val},
		KindCase{'f3', .simple_val},
		KindCase{'f820', .simple_val},
		KindCase{'f4', .bool_val}, // false
		KindCase{'f5', .bool_val}, // true
		KindCase{'f6', .null_val},
		KindCase{'f7', .undefined},
		KindCase{'f93c00', .float_val}, // half
		KindCase{'fa3f800000', .float_val}, // single
		KindCase{'fb3ff0000000000000', .float_val}, // double
		KindCase{'ff', .break_code},
	]
	for c in cases {
		mut u := cbor.new_unpacker(b(c.hexs), cbor.DecodeOpts{})
		got := u.peek_kind() or { panic('peek_kind(${c.hexs})') }
		assert got == c.kind, 'peek_kind(${c.hexs}): got ${got}, want ${c.kind}'
		assert u.pos == 0, 'peek_kind(${c.hexs}) must not advance the cursor'
	}
}

fn test_peek_kind_at_end_of_input_is_an_error() {
	mut u := cbor.new_unpacker([]u8{}, cbor.DecodeOpts{})
	if _ := u.peek_kind() {
		assert false, 'peek_kind on empty input must error'
	}
}

// ---------------------------------------------------------------------
// Unpacker: unpack_bool / unpack_null
// ---------------------------------------------------------------------

fn test_unpack_bool_reads_both_values() {
	mut u := cbor.new_unpacker(b('f4f5'), cbor.DecodeOpts{})
	assert u.unpack_bool() or { panic('f4') } == false
	assert u.unpack_bool() or { panic('f5') }
}

fn test_unpack_bool_rolls_back_on_a_type_mismatch() {
	mut u := cbor.new_unpacker(b('0100'), cbor.DecodeOpts{})
	if _ := u.unpack_bool() {
		assert false, '0x01 is not a CBOR bool'
	}
	assert u.pos == 0, 'a mismatch must leave the cursor where it was'
	// The rolled-back cursor makes the documented branch-then-read pattern work.
	assert u.unpack_uint() or { panic('uint after bool mismatch') } == 1
}

fn test_unpack_null_reads_and_rolls_back() {
	mut ok := cbor.new_unpacker(b('f6f6'), cbor.DecodeOpts{})
	ok.unpack_null() or { panic('f6 must read as null') }
	assert ok.remaining() == 1

	mut bad := cbor.new_unpacker(b('0100'), cbor.DecodeOpts{})
	if _ := bad.unpack_null() {
		assert false, '0x01 is not null'
	}
	assert bad.pos == 0, 'a mismatch must leave the cursor where it was'
	assert bad.unpack_uint() or { panic('uint after null mismatch') } == 1
}

// ---------------------------------------------------------------------
// Unpacker: unpack_float
// ---------------------------------------------------------------------

fn test_unpack_float_accepts_every_ieee_width() {
	mut half := cbor.new_unpacker(b('f93c00'), cbor.DecodeOpts{})
	assert half.unpack_float() or { panic('half 1.0') } == 1.0
	mut half2 := cbor.new_unpacker(b('f93e00'), cbor.DecodeOpts{})
	assert half2.unpack_float() or { panic('half 1.5') } == 1.5

	mut single := cbor.new_unpacker(b('fa3f800000'), cbor.DecodeOpts{})
	assert single.unpack_float() or { panic('single 1.0') } == 1.0

	mut double := cbor.new_unpacker(b('fb3ff0000000000000'), cbor.DecodeOpts{})
	assert double.unpack_float() or { panic('double 1.0') } == 1.0
}

fn test_unpack_float_reads_specials() {
	mut ninf := cbor.new_unpacker(b('f9fc00'), cbor.DecodeOpts{})
	neg := ninf.unpack_float() or { panic('half -inf') }
	assert neg == f64(math.inf(-1)), 'half -inf must read as negative infinity'

	mut pinf := cbor.new_unpacker(b('fb7ff0000000000000'), cbor.DecodeOpts{})
	pos := pinf.unpack_float() or { panic('double +inf') }
	assert pos == f64(math.inf(1)), 'double +inf must read as positive infinity'

	mut dnan := cbor.new_unpacker(b('fb7ff8000000000000'), cbor.DecodeOpts{})
	qnan := dnan.unpack_float() or { panic('double nan') }
	assert qnan != qnan, 'a quiet NaN is not equal to itself'
}

fn test_unpack_float_rolls_back_on_a_type_mismatch() {
	mut u := cbor.new_unpacker(b('01'), cbor.DecodeOpts{})
	if _ := u.unpack_float() {
		assert false, '0x01 is not a float'
	}
	assert u.pos == 0, 'a mismatch must leave the cursor where it was'
	assert u.unpack_uint() or { panic('uint after float mismatch') } == 1
}

// ---------------------------------------------------------------------
// Unpacker: unpack_simple
// ---------------------------------------------------------------------

struct SimpleCase {
	hexs string
	want u8
}

fn test_unpack_simple_reads_the_immediate_and_escaped_forms() {
	// Major type 7 with additional info 0..23 carries the value directly.
	mut e0 := cbor.new_unpacker(b('e0'), cbor.DecodeOpts{})
	assert e0.unpack_simple() or { panic('e0') } == 0
	mut f3 := cbor.new_unpacker(b('f3'), cbor.DecodeOpts{})
	assert f3.unpack_simple() or { panic('f3') } == 19
	// The four decoded simple values report their own numbers.
	for c in [SimpleCase{'f4', 20}, SimpleCase{'f5', 21}, SimpleCase{'f6', 22}, SimpleCase{'f7', 23}] {
		mut u := cbor.new_unpacker(b(c.hexs), cbor.DecodeOpts{})
		assert u.unpack_simple() or { panic('${c.hexs}') } == c.want, c.hexs
	}
	// The one-byte escaped form covers 32..255.
	mut f820 := cbor.new_unpacker(b('f820'), cbor.DecodeOpts{})
	assert f820.unpack_simple() or { panic('f820') } == 32
	mut f8ff := cbor.new_unpacker(b('f8ff'), cbor.DecodeOpts{})
	assert f8ff.unpack_simple() or { panic('f8ff') } == 255
}

fn test_unpack_simple_rejects_values_below_32_in_escaped_form() {
	// RFC 8949: 0..19 must use the immediate form, so f8 1f is malformed.
	mut u := cbor.new_unpacker(b('f81f'), cbor.DecodeOpts{})
	if _ := u.unpack_simple() {
		assert false, 'f81f must be rejected'
	}
}

fn test_unpack_simple_rejects_a_float_initial_byte() {
	mut u := cbor.new_unpacker(b('f93c00'), cbor.DecodeOpts{})
	if _ := u.unpack_simple() {
		assert false, 'a half-float header is not a simple value'
	}
	assert u.pos == 0, 'a mismatch must leave the cursor where it was'
}

// ---------------------------------------------------------------------
// Unpacker: unpack_int / unpack_int_full
// ---------------------------------------------------------------------

struct IntExpect {
	hexs string
	want i64
}

fn test_unpack_int_reads_both_integer_major_types() {
	cases := [
		IntExpect{'00', 0},
		IntExpect{'01', 1},
		IntExpect{'17', 23},
		IntExpect{'1818', 24}, // 1-byte argument
		IntExpect{'18ff', 255},
		IntExpect{'190100', 256}, // 2-byte argument
		IntExpect{'1affffffff', 4294967295}, // 4-byte argument
		IntExpect{'1b0000000100000000', 4294967296}, // 8-byte argument
	]
	for c in cases {
		mut u := cbor.new_unpacker(b(c.hexs), cbor.DecodeOpts{})
		got := u.unpack_int() or { panic('unpack_int(${c.hexs})') }
		assert got == c.want, 'unpack_int(${c.hexs}): got ${got}, want ${c.want}'
	}
}

fn test_unpack_int_negatives_are_minus_one_minus_the_argument() {
	for c in [IntExpect{'20', -1}, IntExpect{'37', -24}, IntExpect{'3863', -100}] {
		mut u := cbor.new_unpacker(b(c.hexs), cbor.DecodeOpts{})
		got := u.unpack_int() or { panic('unpack_int(${c.hexs})') }
		assert got == c.want, 'unpack_int(${c.hexs}): got ${got}, want ${c.want}'
	}
}

struct IntCase {
	hexs      string
	negative  bool
	magnitude u64
}

fn test_unpack_int_full_splits_sign_and_magnitude() {
	cases := [
		IntCase{'00', false, 0},
		IntCase{'01', false, 1},
		IntCase{'1bffffffffffffffff', false, u64(18446744073709551615)},
		IntCase{'20', true, 0},
		IntCase{'37', true, 23},
		IntCase{'3bffffffffffffffff', true, u64(18446744073709551615)},
	]
	for c in cases {
		mut u := cbor.new_unpacker(b(c.hexs), cbor.DecodeOpts{})
		negative, magnitude := u.unpack_int_full() or { panic('unpack_int_full(${c.hexs})') }
		assert negative == c.negative, 'sign for ${c.hexs}'
		assert magnitude == c.magnitude, 'magnitude for ${c.hexs}'
	}
}

fn test_unpack_int_rejects_a_non_integer_initial_byte() {
	mut u := cbor.new_unpacker(b('f6'), cbor.DecodeOpts{})
	if _ := u.unpack_int() {
		assert false, 'null is not an integer'
	}
	assert u.pos == 0, 'a mismatch must leave the cursor where it was'
}

// ---------------------------------------------------------------------
// Unpacker: break codes
// ---------------------------------------------------------------------

fn test_peek_break_looks_without_consuming() {
	mut u := cbor.new_unpacker(b('ff'), cbor.DecodeOpts{})
	assert u.peek_break()
	assert u.pos == 0, 'peek_break must not advance the cursor'

	mut other := cbor.new_unpacker(b('00'), cbor.DecodeOpts{})
	assert other.peek_break() == false
	assert other.pos == 0

	empty := cbor.new_unpacker([]u8{}, cbor.DecodeOpts{})
	assert empty.peek_break() == false
}

fn test_expect_break_consumes_a_break_and_errors_otherwise() {
	mut ok := cbor.new_unpacker(b('ff'), cbor.DecodeOpts{})
	ok.expect_break() or { panic('0xff must be accepted') }
	assert ok.done()

	mut bad := cbor.new_unpacker(b('00'), cbor.DecodeOpts{})
	if _ := bad.expect_break() {
		assert false, '0x00 is not a break code'
	}
	// NOTE: expect_break reads the byte before rejecting it, so unlike
	// unpack_bool it does not roll the cursor back.
	assert bad.pos == 1
}

// ---------------------------------------------------------------------
// Packer: booleans, floats and buffer controls
// ---------------------------------------------------------------------

fn test_pack_bool_emits_the_two_simple_value_bytes() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_bool(false)
	assert hx(p.bytes()) == 'f4'
	p.pack_bool(true)
	assert hx(p.bytes()) == 'f4f5'
}

fn test_pack_float_picks_the_shortest_preserving_width() {
	// Values exactly representable as half collapse to 3 bytes.
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_float(1.0)
	assert hx(p.bytes()) == 'f93c00', '1.0 must encode as a half'
	p.reset()
	p.pack_float(1.5)
	assert hx(p.bytes()) == 'f93e00', '1.5 must encode as a half'
	p.reset()
	p.pack_float(65504.0) // the largest finite half
	assert hx(p.bytes()) == 'f97bff', '65504.0 is still a half'

	// A value with no half reading falls to single, then to double.
	p.reset()
	p.pack_float(0.1)
	assert hx(p.bytes()) == 'fb3fb999999999999a', '0.1 must encode as a double'

	// Specials always collapse to a half, including NaN, which is normalised
	// to the quiet NaN rather than reproducing the input payload.
	p.reset()
	p.pack_float(f64(math.inf(1)))
	assert hx(p.bytes()) == 'f97c00'
	p.reset()
	p.pack_float(f64(math.inf(-1)))
	assert hx(p.bytes()) == 'f9fc00'
	p.reset()
	p.pack_float(f64(math.nan()))
	assert hx(p.bytes()) == 'f97e00'
}

fn test_pack_float32_and_float16_bits_are_width_faithful() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_float32(f32(1.0))
	assert hx(p.bytes()) == 'fa3f800000'
	p.reset()
	p.pack_float16_bits(u16(0x3c00))
	assert hx(p.bytes()) == 'f93c00'
}

fn test_reset_empties_the_buffer() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_bool(true)
	p.pack_uint(7)
	assert p.bytes().len == 2
	p.reset()
	assert p.bytes().len == 0
	// The packer is reusable afterwards.
	p.pack_bool(false)
	assert hx(p.bytes()) == 'f4'
}

fn test_reserve_allocates_without_emitting_bytes() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	assert p.bytes().len == 0
	p.reserve(0)
	assert p.bytes().len == 0
	p.reserve(-1)
	assert p.bytes().len == 0
	p.reserve(4096)
	assert p.bytes().len == 0, 'reserve must not write anything'
	// A large write straight after a reserve still produces correct bytes.
	p.pack_bool(true)
	assert hx(p.bytes()) == 'f5'
	p.reset()
	for i in 0 .. 200 {
		p.pack_uint(u64(i))
	}
	assert p.bytes().len > 100, 'many writes must still accumulate'
}
