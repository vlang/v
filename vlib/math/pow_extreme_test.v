module math

import strconv

fn pow_oracle_bits(text string) u64 {
	return strconv.parse_uint(text, 16, 64) or { panic(err) }
}

fn pow_oracle_close(actual f64, expected f64) bool {
	if is_nan(expected) {
		return is_nan(actual)
	}
	if is_inf(expected, 0) || expected == 0 {
		return f64_bits(actual) == f64_bits(expected)
	}
	if is_nan(actual) || is_inf(actual, 0) || signbit(actual) != signbit(expected) {
		return false
	}
	a := f64_bits(actual) & 0x7fffffffffffffff
	b := f64_bits(expected) & 0x7fffffffffffffff
	return if a > b { a - b <= 4 } else { b - a <= 4 }
}

fn test_pow_independent_mpfr_oracles() {
	fixture := $embed_file('testdata/pow_bits.tsv').to_string()
	mut count := 0
	for row in fixture.split_into_lines() {
		if row.len == 0 || row[0] == `#` {
			continue
		}
		fields := row.split('\t')
		assert fields.len == 3
		x := f64_from_bits(pow_oracle_bits(fields[0]))
		y := f64_from_bits(pow_oracle_bits(fields[1]))
		expected := f64_from_bits(pow_oracle_bits(fields[2]))
		// Exercise the portable kernel even when this package's public pow
		// is overridden by the C source family in a hosted test build.
		actual := pow_software(x, y)
		assert pow_oracle_close(actual, expected), 'row ${count}, pow(${x}, ${y}): ${actual} != ${expected}'
		count++
	}
	assert count == 854
}

fn test_pow_integer_parity_stays_in_the_binary64_integer_range() {
	assert is_odd_int(9007199254740991.0)
	assert is_odd_int(-9007199254740991.0)
	for value in [9007199254740992.0, -9007199254740992.0, max_f64, -max_f64, inf(1), inf(-1),
		nan()] {
		assert !is_odd_int(value)
	}
}
