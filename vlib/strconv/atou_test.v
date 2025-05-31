// Copyright (c) 2019-2024 V language community. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
import strconv

// Perform tests against basic check done on fn test_atou_common_check()
// used from atou_common.
fn test_atou_common_check() {
	// Parsing of these strings should fail on all types.
	ko := [
		'', // Empty string
		'+', // Only sign
		'-10', // - sign
		'_', // Only Underscore
		'_10', // Start with underscore
		'+_10', // Start with underscore after sign.
		'-_16', // Start with underscore after sign.
		'123_', // End with underscore
		'+12_3_', // Sign with trailing underscore
	]

	for v in ko {
		if r := strconv.atou(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string "${v}" should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for : "${v}')
			assert true
		}
	}
}

// Performs tests on possible errors from atou_common function.
// Function called from atou_common are tested above.
fn test_atou_common() {
	struct StrUint {
		str_value  string
		uint_value u64
	}

	ok := [
		StrUint{'0', 0},
		StrUint{'+0', 0},
		StrUint{'1', 1},
		StrUint{'+3_14159', 314159},
		StrUint{'1_00_1', 1001},
		StrUint{'+1_024', 1024},
		StrUint{'123_456_789', 123456789},
		StrUint{'00000006', 6},
		StrUint{'+0_0_0_0_0_0_0_6', 6},
		StrUint{'2147483647', 2147483647},
		StrUint{'+4294967295', 4294967295}, // max u32 bits
		StrUint{'+18446744073709551615', 18446744073709551615}, // max u64 bits
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atou_common(v.str_value, max_u64)! == v.uint_value
	}

	// Parsing of these values should fail !
	ko := [
		'+1_2A', // Non radix 10 character.
		'++A', // double sign.
		'1__0', // 2 consecutive underscore
		'+18446744073709551616', // u64 overflow by 1.
	]

	for v in ko {
		if r := strconv.atou_common(v, max_u64) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} integer extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for: "${v} -> ${err}')
			assert true
		}
	}
}

// performs numeric (bounds) tests over u8 type.
fn test_atou8() {
	struct StrU8 {
		str_value  string
		uint_value u8
	}

	ok := [
		StrU8{'0', 0},
		StrU8{'+0', 0},
		StrU8{'1', 1},
		StrU8{'+39', 39},
		StrU8{'1_23', 123},
		StrU8{'00000006', 6},
		StrU8{'+0_0_0_0_0_0_0_6', 6},
		StrU8{'255', 255}, // max u8
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atou8(v.str_value)! == v.uint_value
	}

	// Parsing of these values should fail !
	ko := [
		'256', // Overflow by one
		'+65535', // overflow of superior type.
	]

	for v in ko {
		if r := strconv.atou8(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} integer extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for: "${v} -> ${err}')
			assert true
		}
	}
}

// performs numeric (bounds) tests over u16 type.
fn test_atou16() {
	struct StrU16 {
		str_value  string
		uint_value u16
	}

	ok := [
		StrU16{'0', 0},
		StrU16{'+0', 0},
		StrU16{'1', 1},
		StrU16{'+16384', 16384},
		StrU16{'1_23', 123},
		StrU16{'00000006', 6},
		StrU16{'+0_0_0_0_0_0_0_6', 6},
		StrU16{'+3_2_7_6_8', 32768},
		StrU16{'65535', 65535}, // max u16
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atou16(v.str_value)! == v.uint_value
	}

	// Parsing of these values should fail !
	ko := [
		'65536', // Overflow by one
		'+4294967295', // overflow of superior type.
	]

	for v in ko {
		if r := strconv.atou16(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} integer extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for: "${v} -> ${err}')
			assert true
		}
	}
}

// atou method acts actually with u32 boundary. In the future int may be mapped on 32/64bits
// depending on machine architecture. That's why we provide atou/atou32 code and tests.
fn test_atou() {
	struct StrU32 {
		str_value  string
		uint_value u32
	}

	ok := [
		StrU32{'0', 0},
		StrU32{'+0', 0},
		StrU32{'1', 1},
		StrU32{'+3_14159', 314159},
		StrU32{'1_00_1', 1001},
		StrU32{'+1_024', 1024},
		StrU32{'123_456_789', 123456789},
		StrU32{'00000006', 6},
		StrU32{'+0_0_0_0_0_0_0_6', 6},
		StrU32{'2147483647', 2147483647},
		StrU32{'+4294967295', 4294967295}, // max u32 bits
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atou(v.str_value)! == v.uint_value
	}

	// Parsing of these values should fail !
	ko := [
		'4294967296', // Overflow by one
		'+18446744073709551615', // overflow of superior type.
	]

	for v in ko {
		if r := strconv.atou(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} integer extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for: "${v} -> ${err}')
			assert true
		}
	}
}

// performs numeric (bounds) tests over u64 type.
fn test_atou32() {
	struct StrU32 {
		str_value  string
		uint_value u32
	}

	ok := [
		StrU32{'0', 0},
		StrU32{'+0', 0},
		StrU32{'1', 1},
		StrU32{'+3_14159', 314159},
		StrU32{'1_00_1', 1001},
		StrU32{'+1_024', 1024},
		StrU32{'123_456_789', 123456789},
		StrU32{'00000006', 6},
		StrU32{'+0_0_0_0_0_0_0_6', 6},
		StrU32{'2147483647', 2147483647},
		StrU32{'+4294967295', 4294967295}, // max u32 bits
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atou32(v.str_value)! == v.uint_value
	}

	// Parsing of these values should fail !
	ko := [
		'4294967296', // Overflow by one
		'+18446744073709551615', // overflow of superior type.
	]

	for v in ko {
		if r := strconv.atou32(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} integer extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for: "${v} -> ${err}')
			assert true
		}
	}
}

// performs numeric (bounds) tests over u64 type.
fn test_atou64() {
	struct StrU64 {
		str_value  string
		uint_value u64
	}

	ok := [
		StrU64{'0', 0},
		StrU64{'+0', 0},
		StrU64{'1', 1},
		StrU64{'+3_14159', 314159},
		StrU64{'1_00_1', 1001},
		StrU64{'+1_024', 1024},
		StrU64{'123_456_789', 123456789},
		StrU64{'00000006', 6},
		StrU64{'+0_0_0_0_0_0_0_6', 6},
		StrU64{'2147483647', 2147483647},
		StrU64{'+18446744073709551615', 18446744073709551615}, // max u64 bits
	]

	// Check that extracted int value matches its string.
	for v in ok {
		// println('Parsing ${v.str_value} should equals ${v.int_value}')
		assert strconv.atou64(v.str_value)! == v.uint_value
	}

	// Parsing of these values should fail !
	ko := [
		'18446744073709551616', // Overflow by one
		'+184467440214748364773709551615', // Large overflow .
	]

	for v in ko {
		if r := strconv.atou64(v) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} integer extraction should not succeed or be considered as valid ${r}).'
		} else {
			// println('Parsing fails as it should for: "${v} -> ${err}')
			assert true
		}
	}
}
