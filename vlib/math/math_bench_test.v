module main

import math
import benchmark

const max_iter = 1000

fn test_benchmark_acos() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.acos(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_acosh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.acosh(1.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_asin() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.asin(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_asinh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.asinh(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_atan() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.atan(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_atanh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.atanh(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_atan2() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.atan2(0.5, 1)
	}
	bmark.measure(@FN)
}

fn test_benchmark_cbrt() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.cbrt(10)
	}
	bmark.measure(@FN)
}

fn test_benchmark_ceil() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.ceil(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_copysign() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.copysign(0.5, -1.0)
	}
	bmark.measure(@FN)
}

fn test_benchmark_cos() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.cos(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_cosh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.cosh(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_erf() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.erf(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_erfc() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.erfc(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_exp() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.exp(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_expm1() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.expm1(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_exp2() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.exp2(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_abs() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.abs(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_floor() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.floor(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_max() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.max(10, 3)
	}
	bmark.measure(@FN)
}

fn test_benchmark_min() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.min(10, 3)
	}
	bmark.measure(@FN)
}

fn test_benchmark_mod() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.mod(10, 3)
	}
	bmark.measure(@FN)
}

fn test_benchmark_frexp() {
	mut x := 0.0
	mut y := 0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x, y = math.frexp(8)
	}
	bmark.measure(@FN)
}

fn test_benchmark_gamma() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.gamma(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_hypot() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.hypot(3, 4)
	}
	bmark.measure(@FN)
}

fn test_benchmark_ldexp() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.ldexp(0.5, 2)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log_gamma() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.log_gamma(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.log(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log_b() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.log_b(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log1p() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.log1p(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log10() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.log10(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log2() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.log2(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_modf() {
	mut x := 0.0
	mut y := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x, y = math.modf(1.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_nextafter32() {
	mut x := f32(0.0)
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.nextafter32(0.5, 1)
	}
	bmark.measure(@FN)
}

fn test_benchmark_nextafter64() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.nextafter(0.5, 1)
	}
	bmark.measure(@FN)
}

fn test_benchmark_pow_int() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.pow(2, 2)
	}
	bmark.measure(@FN)
}

fn test_benchmark_pow_frac() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.pow(2.5, 1.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_pow10_pos() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.pow10(300)
	}
	bmark.measure(@FN)
}

fn test_benchmark_pow10_neg() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.pow10(-300)
	}
	bmark.measure(@FN)
}

fn test_benchmark_round() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.round(-2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_round_to_even() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.round_to_even(-2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_signbit() {
	mut x := false
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.signbit(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sin() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.sin(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sincos() {
	mut x := 0.0
	mut y := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x, y = math.sincos(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sinh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.sinh(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sqrt_indirect() {
	mut x, y := 0.0, 10.0
	f := math.sqrt
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x += f(y)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sqrt_latency() {
	mut x := 10.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.sqrt(x)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sqrt_indirect_latency() {
	mut x := 10.0
	f := math.sqrt
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = f(x)
	}
	bmark.measure(@FN)
}

fn is_prime(i int) bool {
	// yes, this is a dumb way to write this code,
	// but calling sqrt repeatedly in this way demonstrates
	// the benefit of using a direct sqrt instruction on systems
	// that have one, whereas the obvious loop seems not to
	// demonstrate such a benefit.
	for j := 2; f64(j) <= math.sqrt(f64(i)); j++ {
		if i % j == 0 {
			return false
		}
	}
	return true
}

fn test_benchmark_sqrt_prime() {
	mut x := false
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = is_prime(100003)
	}
	bmark.measure(@FN)
}

fn test_benchmark_tan() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.tan(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_tanh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.tanh(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_trunc() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.trunc(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_f64_bits() {
	mut x := u64(0)
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.f64_bits(-2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_f64_from_bits() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.f64_from_bits(5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_f32_bits() {
	mut x := u32(0)
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.f32_bits(-2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_f32_from_bits() {
	mut x := f32(0.0)
	mut bmark := benchmark.start()
	for i in 0 .. max_iter {
		x = math.f32_from_bits(5)
	}
	bmark.measure(@FN)
}
