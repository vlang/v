module main

fn if_expt(this int) (string, int) {
	inc := 1
	mut count := 0
	thing := if this in [0, 1, 2] {
		count += 1
		'0..2'
	} else if this in [3, 4, 5] {
		count += inc
		'3..5'
	} else {
		'not 0..5'
	}
	return thing, count
}

fn test_main() {
	a, b := if_expt(1)
	assert a == '0..2'
	assert b == 1
	c, d := if_expt(4)
	assert c == '3..5'
	assert d == 1
	e, f := if_expt(7)
	assert e == 'not 0..5'
	assert f == 0
}
