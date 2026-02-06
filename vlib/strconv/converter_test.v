module main

fn test_issue_16448() {
	float_prec := 1.9999999999999999 + 0.0000000000000000005
	assert float_prec > 1.99
	assert float_prec < 2.01
}

fn test_atof_converter() {
	mut float_prec := 1.9999999999999999 + 0.0000000000000000005
	assert float_prec == 2.0
	float_prec = 3.9999999999999999 + 0.0000000000000000005
	assert float_prec == 4.0
	float_prec = -3.9999999999999999 - 0.0000000000000000005
	assert float_prec == -4.0
	float_prec = -1.9999999999999999 - 0.0000000000000000005
	assert float_prec == -2.0
	float_prec = 0.025 + 0.0000000000000000005
	assert float_prec == 0.025
	float_prec = 0.0625 + 0.0000000000000000005
	assert float_prec == 0.0625
	float_prec = 0.0009765625 + 0.0000000000000000005
	assert float_prec > 0.00097656250 && float_prec < 0.00097656251
	float_prec = -0.025 - 0.0000000000000000005
	assert float_prec == -0.025
	float_prec = -0.0625 - 0.0000000000000000005
	assert float_prec == -0.0625
	float_prec = -0.0009765625 - 0.0000000000000000005
	assert float_prec < -0.00097656250 && float_prec > -0.00097656251
}

// Testing all the numbers between -100E6..100E6 takes over a minute with clang on i3
// while for just -1E5..1E5, it takes less than a second.
// const r = i64(100_000_001)
// const r = i64(17_000_001)
const r = i64(100_001)

fn test_renormalisation_inside_converter() {
	for i in -r .. r {
		s := '${i}.9999999999999999999'
		fs := s.f64()
		ifs := i64(fs)
		if i % 10000 == 0 {
			eprintln('>>> s: ${s:30s} | fs: ${fs:20.6f} | ifs: ${ifs:20}')
		}
		if i < 0 && ifs != i - 1 {
			panic('invalid negative  f64 value, i: ${i} | s: ${s} | fs: ${fs} | ifs: ${ifs}')
		}
		if i > 0 && ifs != i + 1 {
			panic('invalid positive  f64 value, i: ${i} | s: ${s} | fs: ${fs} | ifs: ${ifs}')
		}
	}
}
