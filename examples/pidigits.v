import math.big

// pi_digits calculates the digits of Pi, based on https://mail.python.org/pipermail/edu-sig/2012-December/010721.html
fn pi_digits(count int) {
	mut n := count
	mut k, mut a, mut b, mut a1, mut b1 := big.c2, big.c4, big.c1, big.c12, big.c4
	mut p, mut q := k, k
	for n > 0 {
		p, q, k = k * k, (big.c2 * k) + big.c1, k + big.c1
		a, b, a1, b1 = a1, b1, p * a + q * a1, p * b + q * b1
		mut d := a / b
		mut d1 := a1 / b1
		for d == d1 {
			print(d.int())
			n--
			a, a1 = big.c10 * (a % b), big.c10 * (a1 % b1)
			d, d1 = a / b, a1 / b1
			if n <= 0 {
				break
			}
		}
	}
}

pi_digits(arguments()[1] or { '100' }.int())
println('')
