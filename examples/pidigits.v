module main

import os
import math.big

const digits_to_print = os.args[1] or { '1000' }.int()

const zero = big.integer_from_int(0)
const one = big.integer_from_int(1)
const two = big.integer_from_int(2)
const three = big.integer_from_int(3)
const four = big.integer_from_int(4)
const ten = big.integer_from_int(10)

fn main() {
	unbuffer_stdout()
	mut digits_printed := 0
	mut k := one
	mut n1 := four
	mut n2 := three
	mut d := one
	mut u := zero
	mut v := zero
	mut w := zero
	for {
		u = n1 / d
		v = n2 / d
		u_int := u.int()
		v_int := v.int()
		if u_int == v_int {
			print(u_int)
			digits_printed++
			if digits_printed >= digits_to_print {
				println('')
				return
			}
			to_minus := u * ten * d
			n1 = n1 * ten - to_minus
			n2 = n2 * ten - to_minus
		} else {
			k2 := k * two
			u = n1 * (k2 - one)
			v = n2 * two
			w = n1 * (k - one)
			n1 = u + v
			u = n2 * (k + two)
			n2 = w + u
			d = d * (k2 + one)
			k += one
		}
	}
}
