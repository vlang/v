// RFC 8949 Appendix A conformance tests. Each entry is taken verbatim
// from the published `cbor/test-vectors` list. Roundtrip-true entries
// are exercised in both directions (encode-and-compare-bytes plus
// decode-and-compare-value); roundtrip-false entries are decode-only,
// which matches the test-vector flag.
//
// Hex bytes are kept as string literals so the diffs against the RFC
// are obvious when reading the file.
module main

import encoding.cbor
import encoding.hex
import math

fn h(s string) []u8 {
	return hex.decode(s) or { panic('invalid hex: ${s}') }
}

fn bytes_eq(a []u8, b []u8) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

fn assert_encode_uint(v u64, hex_expected string) {
	got := cbor.encode[u64](v, cbor.EncodeOpts{}) or { panic(err) }
	expected := h(hex_expected)
	if !bytes_eq(got, expected) {
		panic('encode u64 ${v}: got ${hex.encode(got)}, want ${hex_expected}')
	}
}

fn assert_encode_int(v i64, hex_expected string) {
	got := cbor.encode[i64](v, cbor.EncodeOpts{}) or { panic(err) }
	expected := h(hex_expected)
	if !bytes_eq(got, expected) {
		panic('encode i64 ${v}: got ${hex.encode(got)}, want ${hex_expected}')
	}
}

// ---------------------------------------------------------------------
// Unsigned integers (major type 0)
// ---------------------------------------------------------------------

fn test_unsigned_zero() {
	assert_encode_uint(0, '00')
}

fn test_unsigned_small() {
	assert_encode_uint(1, '01')
	assert_encode_uint(10, '0a')
	assert_encode_uint(23, '17')
}

fn test_unsigned_one_byte() {
	assert_encode_uint(24, '1818')
	assert_encode_uint(25, '1819')
	assert_encode_uint(100, '1864')
	assert_encode_uint(0xff, '18ff')
}

fn test_unsigned_two_bytes() {
	assert_encode_uint(1000, '1903e8')
	assert_encode_uint(0xffff, '19ffff')
}

fn test_unsigned_four_bytes() {
	assert_encode_uint(1_000_000, '1a000f4240')
	assert_encode_uint(0xffffffff, '1affffffff')
}

fn test_unsigned_eight_bytes() {
	assert_encode_uint(1_000_000_000_000, '1b000000e8d4a51000')
	assert_encode_uint(u64(0xffffffffffffffff), '1bffffffffffffffff')
}

// ---------------------------------------------------------------------
// Negative integers (major type 1)
// ---------------------------------------------------------------------

fn test_negative_small() {
	assert_encode_int(-1, '20')
	assert_encode_int(-10, '29')
}

fn test_negative_one_byte() {
	assert_encode_int(-100, '3863')
}

fn test_negative_two_bytes() {
	assert_encode_int(-1000, '3903e7')
}

fn test_negative_extreme() {
	// -2^63 still fits i64.
	assert_encode_int(-9_223_372_036_854_775_808, '3b7fffffffffffffff')
}

fn test_negative_lower_bound_via_packer() {
	// -2^64 (lower bound of CBOR negative ints) requires the full u64
	// argument and can't be represented as an i64.
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_negative_arg(u64(0xffffffffffffffff))
	assert bytes_eq(p.bytes(), h('3bffffffffffffffff'))
}

// ---------------------------------------------------------------------
// Floats — preferred serialisation (RFC 8949 §4.2.2)
// ---------------------------------------------------------------------

fn assert_encode_float(v f64, hex_expected string) {
	got := cbor.encode[f64](v, cbor.EncodeOpts{}) or { panic(err) }
	expected := h(hex_expected)
	if !bytes_eq(got, expected) {
		panic('encode f64 ${v}: got ${hex.encode(got)}, want ${hex_expected}')
	}
}

fn test_float_zero() {
	assert_encode_float(0.0, 'f90000')
	// -0.0 — distinct bit pattern from +0.0.
	neg_zero := math.f64_from_bits(u64(0x8000000000000000))
	assert_encode_float(neg_zero, 'f98000')
}

fn test_float_simple_values() {
	assert_encode_float(1.0, 'f93c00')
	assert_encode_float(1.5, 'f93e00')
	assert_encode_float(65504.0, 'f97bff')
	assert_encode_float(-4.0, 'f9c400')
}

fn test_float_subnormal_half() {
	// 2^-24 — smallest positive half subnormal (preserved exactly in f32/f64).
	assert_encode_float(5.960464477539063e-08, 'f90001')
	// 2^-14 — smallest positive normal half.
	assert_encode_float(6.103515625e-05, 'f90400')
}

fn test_float_single() {
	assert_encode_float(100000.0, 'fa47c35000')
	assert_encode_float(3.4028234663852886e+38, 'fa7f7fffff')
}

fn test_float_double() {
	assert_encode_float(1.1, 'fb3ff199999999999a')
	assert_encode_float(1.0e+300, 'fb7e37e43c8800759c')
	assert_encode_float(-4.1, 'fbc010666666666666')
}

fn test_float_special_values() {
	assert_encode_float(math.inf(1), 'f97c00')
	assert_encode_float(math.inf(-1), 'f9fc00')
	// NaN — encoder collapses to the canonical quiet NaN.
	got := cbor.encode[f64](math.nan(), cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('f97e00')), 'NaN encoded as ${hex.encode(got)}, want f97e00'
}

// ---------------------------------------------------------------------
// Booleans, null, undefined
// ---------------------------------------------------------------------

fn test_bool_false() {
	got := cbor.encode[bool](false, cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, [u8(0xf4)])
}

fn test_bool_true() {
	got := cbor.encode[bool](true, cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, [u8(0xf5)])
}

fn test_null_via_packer() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_null()
	assert bytes_eq(p.bytes(), [u8(0xf6)])
}

fn test_undefined_via_packer() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_undefined()
	assert bytes_eq(p.bytes(), [u8(0xf7)])
}

fn test_simple_values() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_simple(16) or { panic(err) }
	assert bytes_eq(p.bytes(), h('f0'))

	mut p3 := cbor.new_packer(cbor.EncodeOpts{})
	p3.pack_simple(255) or { panic(err) }
	assert bytes_eq(p3.bytes(), h('f8ff'))
}

// RFC 8949 §3.3: encoder MUST NOT issue two-byte sequences starting
// with 0xf8 and continuing with a byte < 0x20. We refuse to emit such a
// value, and the decoder rejects them on input.
fn test_simple_values_rfc8949_strictness() {
	// Encoding side: pack_simple(24..31) returns an error.
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	if _ := p.pack_simple(24) {
		assert false, 'expected pack_simple(24) to fail'
	}
	if _ := p.pack_simple(31) {
		assert false, 'expected pack_simple(31) to fail'
	}
	// Decoder side: f818 (simple(24) two-byte form) is malformed.
	cbor.decode[cbor.Value](h('f818'), cbor.DecodeOpts{}) or {
		assert err.msg().contains('1-byte form'), 'unexpected: ${err.msg()}'
		return
	}
	assert false, 'expected decoder to reject simple(24) two-byte form'
}

// ---------------------------------------------------------------------
// Byte and text strings
// ---------------------------------------------------------------------

fn test_empty_byte_string() {
	got := cbor.encode[[]u8]([]u8{}, cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('40'))
}

fn test_byte_string_4() {
	got := cbor.encode[[]u8]([u8(0x01), 0x02, 0x03, 0x04], cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('4401020304'))
}

fn test_empty_text_string() {
	got := cbor.encode[string]('', cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('60'))
}

fn test_text_a() {
	got := cbor.encode[string]('a', cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('6161'))
}

fn test_text_ietf() {
	got := cbor.encode[string]('IETF', cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('6449455446'))
}

fn test_text_escaped() {
	got := cbor.encode[string]('"\\', cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('62225c'))
}

fn test_text_utf8_2byte() {
	got := cbor.encode[string]('ü', cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('62c3bc'))
}

fn test_text_utf8_3byte() {
	got := cbor.encode[string]('水', cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('63e6b0b4'))
}

fn test_text_utf8_4byte() {
	got := cbor.encode[string]('𐅑', cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('64f0908591'))
}

// ---------------------------------------------------------------------
// Arrays
// ---------------------------------------------------------------------

fn test_empty_array() {
	got := cbor.encode[[]int]([]int{}, cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('80'))
}

fn test_array_3() {
	got := cbor.encode[[]int]([1, 2, 3], cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('83010203'))
}

fn test_array_nested_via_value() {
	v := cbor.Value(cbor.Array{
		elements: [
			cbor.Value(cbor.new_uint(1)),
			cbor.Value(cbor.Array{
				elements: [cbor.Value(cbor.new_uint(2)), cbor.Value(cbor.new_uint(3))]
			}),
			cbor.Value(cbor.Array{
				elements: [cbor.Value(cbor.new_uint(4)), cbor.Value(cbor.new_uint(5))]
			}),
		]
	})
	got := cbor.encode_value(v, cbor.EncodeOpts{})!
	assert bytes_eq(got, h('8301820203820405'))
}

fn test_array_25_items() {
	mut elements := []u64{cap: 25}
	for i in 0 .. 25 {
		elements << u64(i + 1)
	}
	got := cbor.encode[[]u64](elements, cbor.EncodeOpts{}) or { panic(err) }
	want := '98190102030405060708090a0b0c0d0e0f101112131415161718181819'
	assert bytes_eq(got, h(want))
}

// ---------------------------------------------------------------------
// Maps
// ---------------------------------------------------------------------

fn test_empty_map() {
	got := cbor.encode[map[string]int](map[string]int{}, cbor.EncodeOpts{}) or { panic(err) }
	assert bytes_eq(got, h('a0'))
}

fn test_int_key_map_via_packer() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_map_header(2)
	p.pack_int(1)
	p.pack_int(2)
	p.pack_int(3)
	p.pack_int(4)
	assert bytes_eq(p.bytes(), h('a201020304'))
}

fn test_string_key_map_with_array_value() {
	v := cbor.Value(cbor.Map{
		pairs: [
			cbor.MapPair{
				key:   cbor.Value(cbor.Text{
					value: 'a'
				})
				value: cbor.Value(cbor.new_uint(1))
			},
			cbor.MapPair{
				key:   cbor.Value(cbor.Text{
					value: 'b'
				})
				value: cbor.Value(cbor.Array{
					elements: [cbor.Value(cbor.new_uint(2)), cbor.Value(cbor.new_uint(3))]
				})
			},
		]
	})
	got := cbor.encode_value(v, cbor.EncodeOpts{})!
	assert bytes_eq(got, h('a26161016162820203'))
}

fn test_array_with_map_inside() {
	v := cbor.Value(cbor.Array{
		elements: [
			cbor.Value(cbor.Text{
				value: 'a'
			}),
			cbor.Value(cbor.Map{
				pairs: [
					cbor.MapPair{
						key:   cbor.Value(cbor.Text{
							value: 'b'
						})
						value: cbor.Value(cbor.Text{
							value: 'c'
						})
					},
				]
			}),
		]
	})
	got := cbor.encode_value(v, cbor.EncodeOpts{})!
	assert bytes_eq(got, h('826161a161626163'))
}

// ---------------------------------------------------------------------
// Tags
// ---------------------------------------------------------------------

fn test_tag_date_time() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_tag(0)
	p.pack_text('2013-03-21T20:04:00Z')
	assert bytes_eq(p.bytes(), h('c074323031332d30332d32315432303a30343a30305a'))
}

fn test_tag_epoch_int() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_tag(1)
	p.pack_int(1363896240)
	assert bytes_eq(p.bytes(), h('c11a514b67b0'))
}

fn test_tag_epoch_float() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_tag(1)
	p.pack_float64(1363896240.5)
	assert bytes_eq(p.bytes(), h('c1fb41d452d9ec200000'))
}

fn test_tag_uri() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_tag(32)
	p.pack_text('http://www.example.com')
	assert bytes_eq(p.bytes(), h('d82076687474703a2f2f7777772e6578616d706c652e636f6d'))
}

fn test_tag_unsigned_bignum() {
	// Tag 2 + 9-byte big-endian magnitude for 18446744073709551616 = 2^64.
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_tag(2)
	p.pack_bytes([u8(0x01), 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
	assert bytes_eq(p.bytes(), h('c249010000000000000000'))
}

// ---------------------------------------------------------------------
// Indefinite-length items
// ---------------------------------------------------------------------

fn test_indefinite_byte_string() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_bytes_indef()!
	p.pack_bytes([u8(0x01), 0x02])
	p.pack_bytes([u8(0x03), 0x04, 0x05])
	p.pack_break()!
	assert bytes_eq(p.bytes(), h('5f42010243030405ff'))
}

fn test_indefinite_text_string() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_text_indef()!
	p.pack_text('strea')
	p.pack_text('ming')
	p.pack_break()!
	assert bytes_eq(p.bytes(), h('7f657374726561646d696e67ff'))
}

fn test_indefinite_array() {
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_indef()!
	p.pack_break()!
	assert bytes_eq(p.bytes(), h('9fff'))
}

fn test_indefinite_array_with_definite_inside() {
	// [_ 1, [2, 3], [_ 4, 5]]
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_array_indef()!
	p.pack_int(1)
	p.pack_array_header(2)
	p.pack_int(2)
	p.pack_int(3)
	p.pack_array_indef()!
	p.pack_int(4)
	p.pack_int(5)
	p.pack_break()!
	p.pack_break()!
	assert bytes_eq(p.bytes(), h('9f018202039f0405ffff'))
}

fn test_indefinite_map() {
	// {_ "a": 1, "b": [_ 2, 3]}
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_map_indef()!
	p.pack_text('a')
	p.pack_int(1)
	p.pack_text('b')
	p.pack_array_indef()!
	p.pack_int(2)
	p.pack_int(3)
	p.pack_break()!
	p.pack_break()!
	assert bytes_eq(p.bytes(), h('bf61610161629f0203ffff'))
}

// ---------------------------------------------------------------------
// Decode side: every Appendix A vector decodes to the right value
// ---------------------------------------------------------------------

struct UintCase {
	hex string
	val u64
}

struct IntCase {
	hex string
	val i64
}

fn test_decode_unsigned() {
	for pair in [
		UintCase{
			hex: '00'
			val: 0
		},
		UintCase{
			hex: '17'
			val: 23
		},
		UintCase{
			hex: '1818'
			val: 24
		},
		UintCase{
			hex: '1864'
			val: 100
		},
		UintCase{
			hex: '1903e8'
			val: 1000
		},
		UintCase{
			hex: '1a000f4240'
			val: 1_000_000
		},
		UintCase{
			hex: '1b000000e8d4a51000'
			val: 1_000_000_000_000
		},
		UintCase{
			hex: '1bffffffffffffffff'
			val: u64(0xffffffffffffffff)
		},
	] {
		v := cbor.decode[u64](h(pair.hex), cbor.DecodeOpts{}) or {
			panic('decode ${pair.hex}: ${err}')
		}
		assert v == pair.val, 'decoded ${pair.hex}: got ${v}, want ${pair.val}'
	}
}

fn test_decode_negative() {
	for pair in [
		IntCase{
			hex: '20'
			val: -1
		},
		IntCase{
			hex: '29'
			val: -10
		},
		IntCase{
			hex: '3863'
			val: -100
		},
		IntCase{
			hex: '3903e7'
			val: -1000
		},
		IntCase{
			hex: '3b7fffffffffffffff'
			val: i64(-9_223_372_036_854_775_808)
		},
	] {
		v := cbor.decode[i64](h(pair.hex), cbor.DecodeOpts{}) or {
			panic('decode ${pair.hex}: ${err}')
		}
		assert v == pair.val, 'decoded ${pair.hex}: got ${v}, want ${pair.val}'
	}
}

fn test_decode_floats() {
	// Half precision.
	v := cbor.decode[f64](h('f93c00'), cbor.DecodeOpts{}) or { panic(err) }
	assert v == 1.0
	v2 := cbor.decode[f64](h('f93e00'), cbor.DecodeOpts{}) or { panic(err) }
	assert v2 == 1.5
	v3 := cbor.decode[f64](h('f97bff'), cbor.DecodeOpts{}) or { panic(err) }
	assert v3 == 65504.0
	// Subnormal half.
	v4 := cbor.decode[f64](h('f90001'), cbor.DecodeOpts{}) or { panic(err) }
	assert v4 == 5.960464477539063e-08, 'subnormal half: ${v4}'
	// f32 / f64.
	v5 := cbor.decode[f64](h('fa47c35000'), cbor.DecodeOpts{}) or { panic(err) }
	assert v5 == 100000.0
	v6 := cbor.decode[f64](h('fb3ff199999999999a'), cbor.DecodeOpts{}) or { panic(err) }
	assert v6 == 1.1
	// Inf / -Inf.
	v7 := cbor.decode[f64](h('f97c00'), cbor.DecodeOpts{}) or { panic(err) }
	assert math.is_inf(v7, 1)
	v8 := cbor.decode[f64](h('f9fc00'), cbor.DecodeOpts{}) or { panic(err) }
	assert math.is_inf(v8, -1)
	// NaN.
	v9 := cbor.decode[f64](h('f97e00'), cbor.DecodeOpts{}) or { panic(err) }
	assert math.is_nan(v9)
	// Alternate non-canonical encodings (roundtrip=false but must decode).
	for hex_str in ['fa7f800000', 'fb7ff0000000000000'] {
		val := cbor.decode[f64](h(hex_str), cbor.DecodeOpts{}) or { panic(err) }
		assert math.is_inf(val, 1), '${hex_str} → ${val}'
	}
	for hex_str in ['faff800000', 'fbfff0000000000000'] {
		val := cbor.decode[f64](h(hex_str), cbor.DecodeOpts{}) or { panic(err) }
		assert math.is_inf(val, -1), '${hex_str} → ${val}'
	}
	for hex_str in ['fa7fc00000', 'fb7ff8000000000000'] {
		val := cbor.decode[f64](h(hex_str), cbor.DecodeOpts{}) or { panic(err) }
		assert math.is_nan(val), '${hex_str} → ${val}'
	}
}

fn test_decode_bool_null() {
	bf := cbor.decode[bool](h('f4'), cbor.DecodeOpts{}) or { panic(err) }
	assert bf == false
	bt := cbor.decode[bool](h('f5'), cbor.DecodeOpts{}) or { panic(err) }
	assert bt == true
	v := cbor.decode[cbor.Value](h('f6'), cbor.DecodeOpts{}) or { panic(err) }
	assert v.is_nil()
	u := cbor.decode[cbor.Value](h('f7'), cbor.DecodeOpts{}) or { panic(err) }
	assert u.is_undefined()
}

fn test_decode_simple_extended() {
	// Inline form for simple(16).
	v0 := cbor.decode[cbor.Value](h('f0'), cbor.DecodeOpts{}) or { panic(err) }
	assert v0 is cbor.Simple
	if v0 is cbor.Simple {
		assert v0.value == 16
	}
	// 1-byte form for simple(32) — first valid two-byte simple value.
	mut p := cbor.new_packer(cbor.EncodeOpts{})
	p.pack_simple(32) or { panic(err) }
	v32 := cbor.decode[cbor.Value](p.bytes(), cbor.DecodeOpts{}) or { panic(err) }
	assert v32 is cbor.Simple
	if v32 is cbor.Simple {
		assert v32.value == 32
	}
	// Top of the range.
	v2 := cbor.decode[cbor.Value](h('f8ff'), cbor.DecodeOpts{}) or { panic(err) }
	if v2 is cbor.Simple {
		assert v2.value == 255
	}
}

fn test_decode_strings() {
	a := cbor.decode[string](h('60'), cbor.DecodeOpts{}) or { panic(err) }
	assert a == ''
	b := cbor.decode[string](h('6161'), cbor.DecodeOpts{}) or { panic(err) }
	assert b == 'a'
	c := cbor.decode[string](h('6449455446'), cbor.DecodeOpts{}) or { panic(err) }
	assert c == 'IETF'
	d := cbor.decode[string](h('62c3bc'), cbor.DecodeOpts{}) or { panic(err) }
	assert d == 'ü'
	e := cbor.decode[string](h('63e6b0b4'), cbor.DecodeOpts{}) or { panic(err) }
	assert e == '水'
	f := cbor.decode[string](h('64f0908591'), cbor.DecodeOpts{}) or { panic(err) }
	assert f == '𐅑'
}

fn test_decode_array() {
	v := cbor.decode[[]int](h('83010203'), cbor.DecodeOpts{}) or { panic(err) }
	assert v == [1, 2, 3]
}

fn test_decode_map() {
	v := cbor.decode[map[string]int](h('a26161016162820203'), cbor.DecodeOpts{}) or {
		// This vector has b → [2,3], not int — so this decode should fail.
		assert err.msg().contains('mismatch') || err.msg().contains('overflow')
		return
	}
	_ = v
	assert false, 'expected type mismatch on map[string]int decode of array value'
}

fn test_decode_indefinite_text() {
	// "stream"+"ing"
	v := cbor.decode[string](h('7f657374726561646d696e67ff'), cbor.DecodeOpts{}) or { panic(err) }
	assert v == 'streaming'
}

fn test_decode_indefinite_array() {
	v := cbor.decode[[]int](h('9fff'), cbor.DecodeOpts{}) or { panic(err) }
	assert v == []int{}
	v2 := cbor.decode[[]int](h('9f0102030405060708090a0b0c0d0e0f101112131415161718181819ff'), cbor.DecodeOpts{}) or {
		panic(err)
	}
	mut want := []int{cap: 25}
	for i in 0 .. 25 {
		want << i + 1
	}
	assert v2 == want
}

fn test_decode_indefinite_bytes() {
	v := cbor.decode[[]u8](h('5f42010243030405ff'), cbor.DecodeOpts{}) or { panic(err) }
	assert bytes_eq(v, [u8(0x01), 0x02, 0x03, 0x04, 0x05])
}

// ---------------------------------------------------------------------
// Round-trip for every roundtrip=true vector via the Value tree
// ---------------------------------------------------------------------

const roundtrip_vectors = [
	'00',
	'01',
	'0a',
	'17',
	'1818',
	'1819',
	'1864',
	'1903e8',
	'1a000f4240',
	'1b000000e8d4a51000',
	'1bffffffffffffffff',
	'3bffffffffffffffff',
	'20',
	'29',
	'3863',
	'3903e7',
	'f90000',
	'f98000',
	'f93c00',
	'fb3ff199999999999a',
	'f93e00',
	'f97bff',
	'fa47c35000',
	'fa7f7fffff',
	'fb7e37e43c8800759c',
	'f90001',
	'f90400',
	'f9c400',
	'fbc010666666666666',
	'f97c00',
	'f97e00',
	'f9fc00',
	'f4',
	'f5',
	'f6',
	'f7',
	'f0',
	'f8ff',
	'40',
	'4401020304',
	'60',
	'6161',
	'6449455446',
	'62225c',
	'62c3bc',
	'63e6b0b4',
	'64f0908591',
	'80',
	'83010203',
	'8301820203820405',
	'98190102030405060708090a0b0c0d0e0f101112131415161718181819',
	'a0',
	'a201020304',
	'a26161016162820203',
	'826161a161626163',
	'a56161614161626142616361436164614461656145',
]

fn test_value_roundtrip_all_canonical_vectors() {
	for hex_str in roundtrip_vectors {
		input := h(hex_str)
		decoded := cbor.decode[cbor.Value](input, cbor.DecodeOpts{}) or {
			panic('decode ${hex_str}: ${err}')
		}
		got := cbor.encode_value(decoded, cbor.EncodeOpts{})!
		assert bytes_eq(got, input), 'roundtrip ${hex_str}: got ${hex.encode(got)}'
	}
}

// Tag-bearing vectors round-trip via the Value tree as well.
fn test_value_roundtrip_tag_vectors() {
	for hex_str in [
		'c074323031332d30332d32315432303a30343a30305a',
		'c11a514b67b0',
		'c1fb41d452d9ec200000',
		'd74401020304',
		'd818456449455446',
		'd82076687474703a2f2f7777772e6578616d706c652e636f6d',
		'c249010000000000000000',
		'c349010000000000000000',
	] {
		input := h(hex_str)
		decoded := cbor.decode[cbor.Value](input, cbor.DecodeOpts{}) or {
			panic('decode ${hex_str}: ${err}')
		}
		got := cbor.encode_value(decoded, cbor.EncodeOpts{})!
		assert bytes_eq(got, input), 'tag roundtrip ${hex_str}: got ${hex.encode(got)}'
	}
}
