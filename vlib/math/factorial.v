module math

// factorial calculates the factorial of the provided value.
pub fn factorial(n f64) f64 {
	// For a large postive argument (n >= factorials_table.len) return max_f64
	if n >= factorials_table.len {
		return max_f64
	}
	// Otherwise return n!.
	if n == f64(i64(n)) && n >= 0.0 {
		return factorials_table[i64(n)]
	}
	return gamma(n + 1.0)
}

// log_factorial calculates the log-factorial of the provided value.
pub fn log_factorial(n f64) f64 {
	// For a large postive argument (n < 0) return max_f64
	if n < 0 {
		return -max_f64
	}
	// If n < N then return ln(n!).
	if n != f64(i64(n)) {
		return log_gamma(n + 1)
	} else if n < log_factorials_table.len {
		return log_factorials_table[i64(n)]
	}
	// Otherwise return asymptotic expansion of ln(n!).
	return log_factorial_asymptotic_expansion(int(n))
}

fn log_factorial_asymptotic_expansion(n int) f64 {
	m := 6
	mut term := []f64{}
	xx := f64((n + 1) * (n + 1))
	mut xj := f64(n + 1)
	log_factorial := log_sqrt_2pi - xj + (xj - 0.5) * log(xj)
	mut i := 0
	for i = 0; i < m; i++ {
		term << bernoulli[i] / xj
		xj *= xx
	}
	mut sum := term[m - 1]
	for i = m - 2; i >= 0; i-- {
		if abs(sum) <= abs(term[i]) {
			break
		}
		sum = term[i]
	}
	for i >= 0 {
		sum += term[i]
		i--
	}
	return log_factorial + sum
}

// factoriali returns 1 for n <= 0 and -1 if the result is too large for a 64 bit integer
pub fn factoriali(n int) i64 {
	if n <= 0 {
		return i64(1)
	}

	if n < 21 {
		return i64(factorials_table[n])
	}

	return i64(-1)
}
