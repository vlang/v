module math

// catalan returns the n-th Catalan number.
pub fn catalan(n int) i64 {
	if n < 0 {
		return 0
	}
	// C_{i+1} = C_i * 2*(2*i+1)/(i+2). Cancel before multiplying so the
	// running value stays the exact Catalan number and never overflows
	// when the final result fits (this also avoids materializing the
	// oversized C(2n,n) before its final division).
	mut res := i64(1)
	for i in 0 .. n {
		num := i64(2 * (2 * i + 1))
		den := i64(i + 2)
		g1 := gcd(res, den)
		res_div := res / g1
		den_div := den / g1
		g2 := gcd(num, den_div)
		num_div := num / g2
		den_final := den_div / g2
		res = res_div / den_final * num_div
	}
	return res
}
