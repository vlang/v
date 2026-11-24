module complex

import math
import strconv

fn oracle_float(hex string) f64 {
	return math.f64_from_bits(strconv.parse_uint(hex, 16, 64) or { panic(err) })
}

fn oracle_close(actual f64, expected f64) bool {
	if math.is_nan(expected) {
		return math.is_nan(actual)
	}
	if actual == expected {
		return expected != 0 || math.signbit(actual) == math.signbit(expected)
	}
	if math.is_nan(actual) || math.is_inf(actual, 0) || math.is_inf(expected, 0) {
		return false
	}
	return math.abs(actual - expected) <= math.max(math.abs(expected) * 4e-13, math.f64_from_bits(2))
}

fn oracle_operation(z Complex, operation int, power Complex) Complex {
	return match operation {
		0 { z.exp() }
		1 { z.ln() }
		2 { z.sin() }
		3 { z.cos() }
		4 { z.tan() }
		5 { z.asin() }
		6 { z.acos() }
		7 { z.atan() }
		8 { z.sqrt() }
		9 { z.asinh() }
		10 { z.acosh() }
		11 { z.atanh() }
		12 { z.sinh() }
		13 { z.cosh() }
		14 { z.tanh() }
		else { z.cpow(power) }
	}
}

fn test_elementary_independent_mpc_oracles() {
	fixture := $embed_file('testdata/elementary.tsv').to_string()
	mut count := 0
	for row in fixture.split_into_lines() {
		if row.len == 0 || row[0] == `#` {
			continue
		}
		fields := row.split('\t')
		assert fields.len == 7, row
		operation := fields[0].int()
		z := Complex{oracle_float(fields[1]), oracle_float(fields[2])}
		power := Complex{oracle_float(fields[3]), oracle_float(fields[4])}
		expected := Complex{oracle_float(fields[5]), oracle_float(fields[6])}
		actual := oracle_operation(z, operation, power)
		if operation == 15 && !math.is_inf(expected.re, 0) && !math.is_inf(expected.im, 0) {
			// General powers pass a rounded phase to sine and cosine. At an
			// axis crossing, measure their error against the result's magnitude.
			tolerance := math.max(math.max(math.abs(expected.re), math.abs(expected.im)) * 4e-13, math.f64_from_bits(2))
			assert math.abs(actual.re - expected.re) <= tolerance, 'row ${count} power real ${actual.re} != ${expected.re}'
			assert math.abs(actual.im - expected.im) <= tolerance, 'row ${count} power imag ${actual.im} != ${expected.im}'
			count++
			continue
		}
		assert oracle_close(actual.re, expected.re), 'row ${count} op ${operation} (${z.re},${z.im}) real ${actual.re} != ${expected.re}'
		assert oracle_close(actual.im, expected.im), 'row ${count} op ${operation} (${z.re},${z.im}) imag ${actual.im} != ${expected.im}'
		count++
	}
	assert count == 1718
}
