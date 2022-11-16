import math

struct Sample {
	value   f64
	literal f64
}

fn s(literal f64, value f64) Sample {
	return Sample{value, literal}
}

fn main() {
	mut samples := [
		s(1.0000000000000000, 1_000_000.0 * 10.0 / 10_000_000.0),
		s(0.3333333333333300, 1.0 / 3.0),
		s(2.5000000000000000, 10 / 4.0),
		s(0.6666666666666600, 2.0 / 3.0),
		s(0.1000000000000000, 1 / 10.0),
		s(3.1415926535897932, math.pi),
	]
	mut fails := 0
	for sample in samples {
		equal := math.abs(sample.value - sample.literal) < 0.00000000001
		eprintln('> sample.value: ${sample.value:20.16f} | sample.literal: ${sample.literal:20.16f} | equal: ${equal}')
		if !equal {
			fails++
		}
	}
	eprintln('> FAILS: ${fails} .')
}
