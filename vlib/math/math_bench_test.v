module math

import benchmark

const max_iter = 1000

fn test_benchmark_acos() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = acos(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_acosh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = acosh(1.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_asin() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = asin(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_asinh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = asinh(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_atan() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = atan(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_atanh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = atanh(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_atan2() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = atan2(0.5, 1)
	}
	bmark.measure(@FN)
}

fn test_benchmark_cbrt() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = cbrt(10)
	}
	bmark.measure(@FN)
}

fn test_benchmark_ceil() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = ceil(0.5)
	}
	bmark.measure(@FN)
}

const copysign_neg = -1.0

fn test_benchmark_copysign() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = copysign(0.5, math.copysign_neg)
	}
	bmark.measure(@FN)
}

fn test_benchmark_cos() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = cos(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_cosh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = cosh(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_erf() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = erf(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_erfc() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = erfc(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_exp() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = exp(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_expm1() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = expm1(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_exp2() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = exp2(0.5)
	}
	bmark.measure(@FN)
}

const abs_pos = 0.5

fn test_benchmark_abs() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = abs(math.abs_pos)
	}
	bmark.measure(@FN)
}

fn test_benchmark_floor() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = floor(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_max() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = max(10, 3)
	}
	bmark.measure(@FN)
}

fn test_benchmark_min() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = min(10, 3)
	}
	bmark.measure(@FN)
}

fn test_benchmark_mod() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = mod(10, 3)
	}
	bmark.measure(@FN)
}

fn test_benchmark_frexp() {
	mut x := 0.0
	mut y := 0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x, y = frexp(8)
	}
	bmark.measure(@FN)
}

fn test_benchmark_gamma() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = gamma(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_hypot() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = hypot(3, 4)
	}
	bmark.measure(@FN)
}

fn test_benchmark_ldexp() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = ldexp(0.5, 2)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log_gamma() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = log_gamma(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = log(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log_b() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = log_b(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log1p() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = log1p(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log10() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = log10(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_log2() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = log2(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_modf() {
	mut x := 0.0
	mut y := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x, y = modf(1.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_nextafter32() {
	mut x := f32(0.0)
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = nextafter32(0.5, 1)
	}
	bmark.measure(@FN)
}

fn test_benchmark_nextafter64() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = nextafter(0.5, 1)
	}
	bmark.measure(@FN)
}

fn test_benchmark_pow_int() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = pow(2, 2)
	}
	bmark.measure(@FN)
}

fn test_benchmark_pow_frac() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = pow(2.5, 1.5)
	}
	bmark.measure(@FN)
}

const pow10pos = int(300)

fn test_benchmark_pow10_pos() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = pow10(math.pow10pos)
	}
	bmark.measure(@FN)
}

const pow10neg = int(-300)

fn test_benchmark_pow10_neg() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = pow10(math.pow10neg)
	}
	bmark.measure(@FN)
}

const round_neg = f64(-2.5)

fn test_benchmark_round() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = round(math.round_neg)
	}
	bmark.measure(@FN)
}

fn test_benchmark_round_to_even() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = round_to_even(math.round_neg)
	}
	bmark.measure(@FN)
}

const signbit_pos = 2.5

fn test_benchmark_signbit() {
	mut x := false
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = signbit(math.signbit_pos)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sin() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = sin(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sincos() {
	mut x := 0.0
	mut y := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x, y = sincos(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sinh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = sinh(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sqrt_indirect() {
	mut x, y := 0.0, 10.0
	f := sqrt
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x += f(y)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sqrt_latency() {
	mut x := 10.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = sqrt(x)
	}
	bmark.measure(@FN)
}

fn test_benchmark_sqrt_indirect_latency() {
	mut x := 10.0
	f := sqrt
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
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
	for j := 2; f64(j) <= sqrt(f64(i)); j++ {
		if i % j == 0 {
			return false
		}
	}
	return true
}

fn test_benchmark_sqrt_prime() {
	mut x := false
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = is_prime(100003)
	}
	bmark.measure(@FN)
}

fn test_benchmark_tan() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = tan(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_tanh() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = tanh(2.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_trunc() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = trunc(0.5)
	}
	bmark.measure(@FN)
}

fn test_benchmark_f64_bits() {
	mut y := u64(0)
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		y = f64_bits(math.round_neg)
	}
	bmark.measure(@FN)
}

const round_u64 = u64(5)

fn test_benchmark_f64_from_bits() {
	mut x := 0.0
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = f64_from_bits(math.round_u64)
	}
	bmark.measure(@FN)
}

const round_f32 = f32(-2.5)

fn test_benchmark_f32_bits() {
	mut y := u32(0)
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		y = f32_bits(math.round_f32)
	}
	bmark.measure(@FN)
}

const round_u32 = u32(5)

fn test_benchmark_f32_from_bits() {
	mut x := f32(0.0)
	mut bmark := benchmark.start()
	for i in 0 .. math.max_iter {
		x = f32_from_bits(math.round_u32)
	}
	bmark.measure(@FN)
}
