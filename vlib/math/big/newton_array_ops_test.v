module big

// import benchmark

fn test_add_in_place () {
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
}

fn test_clear_first_bits_and_set_some() {
	mut a := [u32(0xffffffff), 0xffffffff, 0xffffffff, 0xffffffff]
	mut b := []u32{len: 4, init: 0}
	clear_first_bits_and_set_some(a, 16, mut b)
	assert b == [u32(0xffffffff), 0xffffffff, 0xffffffff, 0xffff]

	a = [u32(0xffffffff), 0xffffffff, 0x0000ffff, 0x800]
	b = []u32{len: 4, init: 0}
	clear_first_bits_and_set_some(a, 20, mut b)
	assert b == [u32(0xffffffff), 0xffffffff, 0x00e0ffff, 0x0]
}

fn test_neg_in_place () {
	mut a := [u32(1), 0, 0]
	neg_in_place(mut a)
	assert a == [u32(0xffffffff), 0xffffffff, 0xffffffff]
	a = [u32(2), 0, 0]
	neg_in_place(mut a)
	assert a == [u32(0xfffffffe), 0xffffffff, 0xffffffff]
	a = [u32(3), 0, 0]
	neg_in_place(mut a)
	assert a == [u32(0xfffffffd), 0xffffffff, 0xffffffff]

	a = [u32(0), 0, 0]
	neg_in_place(mut a)
	assert a == [u32(0), 0, 0]

	a = [u32(0), 0, 1]
	neg_in_place(mut a)
	assert a == [u32(0), 0, 0xffffffff]

}

// fn test_substract_2 () {
// 	mut a := Integer{ digits: [u32(1), 2, 3, 4]}
// 	mut b := Integer{ digits: [u32(1), 2]}
// 	mut c := Integer{ digits: []u32{}}
// 	subtract_2(a, b, mut c, 5)
// 	assert c.digits == [u32(0), 0, 3, 4]

// 	a = Integer{signum: 1, digits: [u32(1), 2, 3, 4]}
// 	b = Integer{signum: -1, digits: [u32(1), 2]}
// 	subtract_2(a, b, mut c, 5)
// 	assert c.digits == [u32(2), 4, 3, 4]

// 	a = Integer{signum: 1, digits: [u32(1), 2]}
// 	b = Integer{signum: 1, digits: [u32(1), 2, 3, 4]}
// 	subtract_2(a, b, mut c, 5)
// 	assert c.digits == [u32(0), 0, 3, 4]
// 	assert c.signum == -1
// }

// fn test_multiply_2 () {
// 	mut a := Integer{digits: [u32(0), 0, 0, 1]}
// 	mut b := Integer{digits: [u32(0), 0, 1]}
// 	mut c := Integer{digits: []u32{}}
// 	multiply_2(a, b, mut c, 7)

// 	assert c.digits == [u32(0), 0, 0, 0, 0, 1]

// 	a = zero_int
// 	b = Integer{digits: [u32(0), 0, 1]}
// 	multiply_2(a, b, mut c, 4)

// 	assert c.digits == []

// 	multiply_2(b, a, mut c, 4)

// 	assert c.digits == []
// }

fn test_multiply_karatsuba () {
	mut a := integer_from_i64(3)
	mut b := integer_from_int(0)
	assert multiply_kara_simpl(a, b) == zero_int

	a = integer_from_i64(3)
	b = integer_from_int(1)
	assert multiply_kara_simpl(a, b) == a

	a = integer_from_i64(0)
	b = integer_from_int(4)
	assert multiply_kara_simpl(a, b) == zero_int

	a = integer_from_i64(1)
	b = integer_from_int(5)
	assert multiply_kara_simpl(a, b) == b

	a = integer_from_i64(3)
	b = integer_from_int(0)
	assert multiply_kara_simpl(a, b) == zero_int

	a = integer_from_i64(1234)
	b = integer_from_int(567)
	assert multiply_kara_simpl(a, b) == integer_from_i64(699_678)

	a = integer_from_string('95484736384949495947362') or {panic('error')}
	b = integer_from_string('39474638493') or {panic('error')}
	assert multiply_kara_simpl(a, b).str() == '3769225450395285038584683507005466'
}

fn test_karatsuba_mult () {
	a := integer_from_string('53575430359313366047421252453000090528070240585276680372187519418517552556246806124659918940784792906379733645877657341259357264284615702179922887873492874019672838874121154927105373025311855709389770910765') or {panic('error')}
	b := integer_from_string('977091076523237491790970633699383779582771973038531457285598238843271083830214915826312193418602834034688531898668229388286706296786321423078510899614439367') or {panic('error')}
	mut c := zero_int
	c = multiply_kara_simpl(a, b)
	assert c.str() == '52348074924977237255285644820010078601114587486470740900886892189662650320988400136613780986308710610258879824881256666730655821800564143426560480113864123642197317383052431412305975584645367703594190956925565749714310612399025459615546540332117815550470167143256687163102859337019449165214274088466835988832405507818643018779158891710706073875995722420460085755'
	c = a * b
	assert c.str() == '52348074924977237255285644820010078601114587486470740900886892189662650320988400136613780986308710610258879824881256666730655821800564143426560480113864123642197317383052431412305975584645367703594190956925565749714310612399025459615546540332117815550470167143256687163102859337019449165214274088466835988832405507818643018779158891710706073875995722420460085755' // expected result calculated through Julia
	// mut bm := benchmark.start()
	// for _ in 0 .. 30 {
	// 	c = multiply_kara_simpl(a, b)
	// }
	// bm.measure('karatsuba_simple')
	// for _ in 0 .. 30 {
	// 	c = a * b
	// }
	// bm.measure('normal algorithm')
}
// results of the test:
//  SPENT     7.103 ms in karatsuba_simple
//  SPENT     0.369 ms in normal algorithm
// Not very efficient algorithm with reentrance

fn test_newton_divide_01 () {
	a := [u32(699_678)]
	b := [u32(1234)]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_array_by_array(a, b, mut q, mut r)
	assert q == [u32(567)]
	assert r == []u32{len: 0}
}

fn test_newton_divide_02 () {
	a := [u32(699_678)]
	b := [u32(567)]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_array_by_array(a, b, mut q, mut r)
	assert q == [u32(1234)]
	assert r == []u32{len: 0}
}

fn test_divide_digit_array_03() {
	a := [u32(0), 4]
	b := [u32(0), 1]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_array_by_array(a, b, mut q, mut r)
	assert q == [u32(4)]
	assert r == []u32{len: 0}
}

fn test_divide_digit_array_04() {
	a := [u32(2), 4]
	b := [u32(0), 1]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u32(4)]
	assert r == [u32(2)]
}

fn test_divide_digit_array_05() {
	a := [u32(2), 4, 5]
	b := [u32(0), 1]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u32(4), 5]
	assert r == [u32(2)]
}

fn test_divide_digit_array_06() {
	a := [u32(2), 4, 5, 3]
	b := [u32(0), 0x8000]
	mut q := []u32{cap: a.len - b.len + 1}
	mut r := []u32{cap: a.len}

	divide_digit_array(a, b, mut q, mut r)
	assert q == [u32(0xa0000), 0x60000]
	assert r == [u32(2), 4]
}

fn test_divide_digit_array_07() {
	a := integer_from_string('52348074924977237255285644820010078601114587486470740900886892189662650320988400136613780986308710610258879824881256666730655821800564143426560480113864123642197317383052431412305975584645367703594190956925565749714310612399025459615546540332117815550470167143256687163102859337019449165214274088466835988832405507818643018779158891710706073875995722420460085757') or {panic('error')}
	b := integer_from_string('977091076523237491790970633699383779582771973038531457285598238843271083830214915826312193418602834034688531898668229388286706296786321423078510899614439367') or {panic('error')}
	mut q := []u32{cap: a.digits.len - b.digits.len + 1}
	mut r := []u32{cap: a.digits.len}

	divide_digit_array(a.digits, b.digits, mut q, mut r)
	quotient := Integer{signum:1, digits: q}
	assert quotient.str() == '53575430359313366047421252453000090528070240585276680372187519418517552556246806124659918940784792906379733645877657341259357264284615702179922887873492874019672838874121154927105373025311855709389770910765'
	assert r == [u32(2)]
}
