module complex

import math
import strconv

#flag linux -lm
#include "@VMODROOT/vlib/math/complex/testdata/elementary_reference.h"

fn C.v_complex_reference(int, f64, f64, f64, f64, &f64, &f64)

fn reference_oracles() map[string]Complex {
	mut oracles := map[string]Complex{}
	fixtures := $embed_file('testdata/elementary.tsv').to_string() + '\n' +
		$embed_file('testdata/elementary_special.tsv').to_string()
	for row in fixtures.split_into_lines() {
		if row.len == 0 || row[0] == `#` {
			continue
		}
		fields := row.split('\t')
		assert fields.len == 7
		if fields[0] == '15' {
			continue
		}
		re := strconv.parse_uint(fields[5], 16, 64) or { panic(err) }
		im := strconv.parse_uint(fields[6], 16, 64) or { panic(err) }
		oracles['${fields[0]}:${fields[1]}:${fields[2]}'] = Complex{math.f64_from_bits(re), math.f64_from_bits(im)}
	}
	return oracles
}

fn elementary_operation(z Complex, operation int, power Complex) Complex {
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

fn reference_close(actual f64, expected f64) bool {
	if math.is_nan(expected) {
		return math.is_nan(actual)
	}
	if math.is_inf(expected, 0) {
		return actual == expected
	}
	if math.is_nan(actual) || math.is_inf(actual, 0) {
		return false
	}
	if expected == 0 {
		return math.abs(actual) <= math.f64_from_bits(8)
	}
	return math.abs(actual - expected) <= math.max(math.abs(expected) * 4e-13, math.f64_from_bits(8))
}

fn compare_reference(z Complex, operation int, power Complex, oracles map[string]Complex) {
	mut rr := 0.0
	mut ri := 0.0
	key := '${operation}:${math.f64_bits(z.re):016x}:${math.f64_bits(z.im):016x}'
	if key in oracles {
		// Prefer independent MPC results and ISO C Annex G expectations where
		// available. Host libms can lose components or disagree at singularities.
		expected := oracles[key]
		rr = expected.re
		ri = expected.im
	} else {
		C.v_complex_reference(operation, z.re, z.im, power.re, power.im, &rr, &ri)
	}
	actual := elementary_operation(z, operation, power)
	// Annex G permits different infinity signs for several nonfinite
	// argument combinations. Finite-input overflow signs remain checked.
	if math.is_inf(z.re, 0) || math.is_inf(z.im, 0) || math.is_nan(z.re) || math.is_nan(z.im) {
		assert reference_close(actual.re, rr) || (math.is_inf(actual.re, 0) && math.is_inf(rr, 0)), 'op ${operation} nonfinite real ${actual.re} != ${rr}'
		assert reference_close(actual.im, ri) || (math.is_inf(actual.im, 0) && math.is_inf(ri, 0)), 'op ${operation} nonfinite imag ${actual.im} != ${ri}'
		return
	}
	// Near an output-axis crossing, a small phase error in either library
	// becomes a large relative component error. Compare powers against the
	// scale of the full result; exact cancellation is covered by static oracles.
	if operation == 15 {
		scale := math.max(math.abs(rr), math.abs(ri))
		assert math.abs(actual.re - rr) <= scale * 4e-13, 'power (${z.re},${z.im})^(${power.re},${power.im}) real ${actual.re} != ${rr}'
		assert math.abs(actual.im - ri) <= scale * 4e-13, 'power (${z.re},${z.im})^(${power.re},${power.im}) imag ${actual.im} != ${ri}'
		return
	}
	assert reference_close(actual.re, rr), 'op ${operation} (${z.re},${z.im}) real ${actual.re} != ${rr}'
	assert reference_close(actual.im, ri), 'op ${operation} (${z.re},${z.im}) imag ${actual.im} != ${ri}'
}

fn test_elementary_special_values_against_host_libm() {
	$if !freestanding {
		oracles := reference_oracles()
		values := [0.0, math.copysign(0, -1), 1.0, -1.0, 0.5, -0.5, math.f64_from_bits(1),
			-math.f64_from_bits(1), 1e-150, -1e-150, 1e150, -1e150, math.max_f64, -math.max_f64,
			math.inf(1), math.inf(-1), math.nan()]
		for re in values {
			for im in values {
				for operation in 0 .. 15 {
					compare_reference(Complex{re, im}, operation, Complex{}, oracles)
				}
			}
		}
	}
}

struct ElementaryRandom {
mut:
	state u64
}

fn (mut r ElementaryRandom) sample() f64 {
	r.state = r.state * 6364136223846793005 + 1442695040888963407
	return f64(r.state >> 11) * (1.0 / 9007199254740992.0)
}

fn test_elementary_finite_values_against_host_libm() {
	$if !freestanding {
		oracles := reference_oracles()
		mut r := ElementaryRandom{ state: 0x76696c655f6d6174 }
		for _ in 0 .. 4000 {
			z := Complex{(r.sample() - 0.5) * 40, (r.sample() - 0.5) * 40}
			power := Complex{(r.sample() - 0.5) * 4, (r.sample() - 0.5) * 4}
			for operation in 0 .. 16 {
				compare_reference(z, operation, power, oracles)
			}
		}
	}
}

fn test_elementary_scaled_and_branch_neighbors_against_host_libm() {
	$if !freestanding {
		oracles := reference_oracles()
		values := [math.f64_from_bits(1), 1e-200, 1e-20, 0.9999999999999999, 1.0, 1.0000000000000002,
			21.999999999999996, 22.0, 709.0, 710.0, 710.4758600739439, 745.0, 1000.0, 1454.0, 1455.0,
			4503599627370496.0]
		for re in values {
			for im in values {
				for sr in [-1.0, 1.0] {
					for si in [-1.0, 1.0] {
						for operation in 0 .. 15 {
							compare_reference(Complex{sr * re, si * im}, operation, Complex{}, oracles)
						}
					}
				}
			}
		}
	}
}
