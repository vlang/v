module big

fn test_add_in_place() {
	mut a := [u32(1), 2, 3]
	mut b := [u32(5), 6, 7]
	add_in_place(mut a, b)
	assert a == [u32(6), 8, 10]

	a = [u32(11), 10, 11, 12]
	b = [u32(1), 2]
	add_in_place(mut a, b)
	assert a == [u32(12), 12, 11, 12]

	a = []u32{cap: 4}
	a << u32(1)
	a << u32(2)
	b = [u32(3), 4, 5, 6]
	add_in_place(mut a, b)
	assert a == [u32(4), 6, 5, 6]

	a = [u32(0x3ce9124b), 0x1438]
	b = [u32(0xdb166062)]
	add_in_place(mut a, b)
	assert a == [u32(0x17ff72ad), 0x1439]
}

fn test_lshift_digits_in_place() {
	mut a := [u32(5), 6, 7, 8]
	lshift_digits_in_place(mut a, 2)
	assert a == [u32(0), 0, 5, 6, 7, 8]
}

fn test_multiply_karatsuba_01() {
	mut a := [u32(3)]
	mut b := []u32{}
	mut c := []u32{len: a.len + b.len, init: 0}
	karatsuba_multiply_digit_array(a, b, mut c)
	assert c == []u32{}

	a = []u32{}
	b = [u32(4)]
	c = []u32{len: a.len + b.len, init: 0}
	karatsuba_multiply_digit_array(a, b, mut c)
	assert c == []u32{}

	a = [u32(3)]
	b = [u32(1)]
	c = []u32{len: a.len + b.len, init: 0}
	karatsuba_multiply_digit_array(a, b, mut c)
	assert c == a

	a = [u32(1)]
	b = [u32(5)]
	c = []u32{len: a.len + b.len, init: 0}
	karatsuba_multiply_digit_array(a, b, mut c)
	assert c == b

	a = [u32(1234)]
	b = [u32(567)]
	c = []u32{len: a.len + b.len + 1, init: 0}
	karatsuba_multiply_digit_array(a, b, mut c)
	assert c == [u32(699_678)]

	a = [u32(0x17ff72ad), 0x1439]
	b = [u32(0x30df2ea6)]
	c = []u32{len: a.len + b.len + 1, init: 0}
	karatsuba_multiply_digit_array(a, b, mut c)
	assert c == [u32(0xcaf2722e), 0x55eb2c5a, 0x3dc]

	a_operand := integer_from_string('95484736384949495947362') or { panic('error') }
	b_operand := integer_from_string('39474638493') or { panic('error') }
	c = []u32{len: a_operand.digits.len + b_operand.digits.len, init: 0}
	karatsuba_multiply_digit_array(a_operand.digits, b_operand.digits, mut c)
	expected := integer_from_string('3769225450395285038584683507005466') or { panic('error') }
	assert c == expected.digits
}

fn test_multiply_karatsuba_02() {
	a := integer_from_string('53575430359313366047421252453000090528070240585276680372187519418517552556246806124659918940784792906379733645877657341259357264284615702179922887873492874019672838874121154927105373025311855709389770910765') or {
		panic('error')
	}
	b := integer_from_string('977091076523237491790970633699383779582771973038531457285598238843271083830214915826312193418602834034688531898668229388286706296786321423078510899614439367') or {
		panic('error')
	}
	mut c := []u32{len: a.digits.len + b.digits.len + 1, init: 0}
	karatsuba_multiply_digit_array(a.digits, b.digits, mut c)
	expected := integer_from_string('52348074924977237255285644820010078601114587486470740900886892189662650320988400136613780986308710610258879824881256666730655821800564143426560480113864123642197317383052431412305975584645367703594190956925565749714310612399025459615546540332117815550470167143256687163102859337019449165214274088466835988832405507818643018779158891710706073875995722420460085755') or {
		panic('error')
	}
}

fn test_newton_divide_03() {
	a := [u32(0), 4]
	b := [u32(0), 1]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	newton_divide_array_by_array(a, b, mut q, mut r)
	assert q == [u32(4)]
	assert r == []u32{len: 0}
}

fn test_newton_divide_04() {
	a := [u32(2), 4]
	b := [u32(0), 1]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	newton_divide_array_by_array(a, b, mut q, mut r)
	assert q == [u32(4)]
	assert r == [u32(2)]
}

fn test_newton_divide_05() {
	a := [u32(2), 4, 5]
	b := [u32(0), 1]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	newton_divide_array_by_array(a, b, mut q, mut r)
	assert q == [u32(4), 5]
	assert r == [u32(2)]
}

fn test_newton_divide_06() {
	a := [u32(2), 4, 5, 3]
	b := [u32(0), 0x8000]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	newton_divide_array_by_array(a, b, mut q, mut r)
	assert q == [u32(0xa0000), 0x60000]
	assert r == [u32(2), 4]
}

fn test_newton_divide_07() {
	a := integer_from_string('52348074924977237255285644820010078601114587486470740900886892189662650320988400136613780986308710610258879824881256666730655821800564143426560480113864123642197317383052431412305975584645367703594190956925565749714310612399025459615546540332117815550470167143256687163102859337019449165214274088466835988832405507818643018779158891710706073875995722420460085757') or {
		panic('error')
	}
	b := integer_from_string('977091076523237491790970633699383779582771973038531457285598238843271083830214915826312193418602834034688531898668229388286706296786321423078510899614439367') or {
		panic('error')
	}
	mut q := []u32{cap: a.digits.len - b.digits.len + 1}
	mut r := []u32{cap: a.digits.len}

	newton_divide_array_by_array(a.digits, b.digits, mut q, mut r)
	quotient := Integer{
		signum: 1
		digits: q
	}
	assert quotient.str() == '53575430359313366047421252453000090528070240585276680372187519418517552556246806124659918940784792906379733645877657341259357264284615702179922887873492874019672838874121154927105373025311855709389770910765'
	assert r == [u32(2)]
}
