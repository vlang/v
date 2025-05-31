import math.big

const d_2 = big.integer_from_int(2)
const d_4 = big.integer_from_int(4)
const d_10 = big.integer_from_int(10)

fn test_dividing_big_numbers() {
	mut n := 250_000
	base := d_10.pow(u32(n - 1))
	eprintln('> n: ${n} | base.bit_len: ${base.bit_len()}')
	a := d_4 * base
	b := d_2 * base
	eprintln('> a.bit_len: ${a.bit_len()} | b.bit_len: ${b.bit_len()}')
	assert a.bit_len() == 830481
	assert b.bit_len() == 830480
	c := a / b // c should be 2
	eprintln('> c: ${c} | c.bit_len(): ${c.bit_len()}')
	assert c.str() == '2'
	assert c.bit_len() == 2
}
