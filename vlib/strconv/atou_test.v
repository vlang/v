import strconv


// Tes
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
		'+12_3_' // Sign with trailing underscore
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


// Performs tests on possible error of atou_common.
// Function called from atou_common are tested above.
fn test_atou_common() {
	struct StrUint {
		str_value string
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
		'++A',   // double sign.
		'1__0', // 2 consecutive underscore
		'+18446744073709551616' // u64 overflow by 1.
	]

	for v in ko {
		if r := strconv.atou_common(v, max_u64) {
			// These conversions should fail so force assertion !
			assert false, 'The string ${v} integer extraction should not succeed or be considered as valid ${r}).'
		} else {
			println('Parsing fails as it should for: "${v} -> ${err}')
			assert true
		}
	}
}


fn test_atou() {

}
