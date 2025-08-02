module main

import math.big
import benchmark

const n = 2500_000

fn dividing_big_numbers_big_pow() {
	d_12 := big.integer_from_int(12)
	d_4 := big.integer_from_int(4)
	d_10 := big.integer_from_int(10)

	mut clock := benchmark.start()
	base := d_10.pow(u32(n - 1))
	clock.measure('math.big> n: ${n} | base.bit_len: ${base.bit_len()}')
	a := d_12 * base
	b := d_4 * base
	clock.measure('math.big> a.bit_len: ${a.bit_len()} | b.bit_len: ${b.bit_len()}')
	c := a / b // c should be 3
	clock.measure('math.big> c: ${c} | c.bit_len(): ${c.bit_len()}')
	if c.str() != '3' {
		println('math.big:error! c.str() == ${c.str()}')
	}
	if c.bit_len() != 2 {
		println('math.big:error! c.bit_len() == ${c.bit_len()}')
	}
}

fn main() {
	dividing_big_numbers_big_pow()
}
