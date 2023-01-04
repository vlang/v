import time
import math.big

const t = time.new_stopwatch()

// a := big.integer_from_i64(2).pow(123123123)
// println(a)

fn timed_println(msg string) {
	timed_println_extended(t, msg)
}

fn timed_println_extended(t time.StopWatch, msg string) {
	println('${t.elapsed().microseconds():12} | ${msg}')
}

fn f(x big.Integer, y int) big.Integer {
	timed_println('f x y,   y=${y:12}')
	if y & 1 == 0 {
		return f(x * x, y / 2)
	}
	if y == 1 {
		return x
	}
	return g(x * x, y / 2, x)
}

fn g(x big.Integer, y int, z big.Integer) big.Integer {
	timed_println('g x y z, y=${y:12}')
	if y & 1 == 0 {
		return g(x * x, y / 2, z)
	}
	if y == 1 {
		return x * z
	}
	return g(x * x, y / 2, x * z)
}

fn calculate_and_measure(calc_label string, cb fn () big.Integer) string {
	sw := time.new_stopwatch()
	timed_println_extended(sw, 'start')
	a := cb()
	timed_println_extended(sw, 'done  ${calc_label}')
	timed_println_extended(sw, 'a.bit_len(): ${a.bit_len():12}')

	timed_println_extended(sw, 'before a.str()')
	// s := a.hex() // hex is very fast since it does not need to do divisions at all
	s := a.str()
	timed_println_extended(sw, 'after  a.str()')
	dump(s.len)
	dump(s#[0..10])
	dump(s#[-10..])
	timed_println_extended(sw, 'finish')
	return s
}

fn test_exponentiation() {
	res1 := calculate_and_measure('f(x, y)', fn () big.Integer {
		return f(big.integer_from_int(2), 66777)
	})
	res2 := calculate_and_measure('big.int(x).pow(y)', fn () big.Integer {
		return big.integer_from_i64(2).pow(66777)
	})
	assert res1 == res2
}
