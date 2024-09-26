fn test_fixed_array_can_be_assigned() {
	x := 2.32
	mut v := [8]f64{}
	assert v[1] == 0
	v = [1.0, x, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]!
	assert v[1] == x
	v[1] = 2.0
	for i, e in v {
		assert e == i + 1
	}
	v = [8]f64{}
	assert v[1] == 0
	// test slicing
	for e in v[0..v.len] {
		assert e == 0
	}
	v = [8]f64{init: 3.0}
	assert v[1] == 3.0
}

fn test_fixed_array_assignment() {
	mut a := [2]int{}
	a[0] = 111
	a[1] = 222
	b := a
	assert b[0] == a[0]
	assert b[1] == a[1]
	mut c := [2]int{}
	c = a
	assert c[0] == a[0]
	assert c[1] == a[1]
	d := [3]int{init: 333}
	for val in d {
		assert val == 333
	}
	e := [3]string{init: 'vlang'}
	for val in e {
		assert val == 'vlang'
	}
}

fn test_fixed_array_can_be_used_in_declaration() {
	x := 2.32
	v := [1.0, x, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]!
	assert v.len == 8
	assert v[1] == x
}

struct Context {
pub mut:
	vb [8]f64
}

fn test_fixed_array_can_be_assigned_to_a_struct_field() {
	mut ctx := Context{}
	assert ctx.vb.len == 8
	x := 2.32
	ctx.vb = [1.1, x, 3.3, 4.4, 5.0, 6.0, 7.0, 8.9]!
	assert ctx.vb[1] == x
	assert ctx.vb[7] == 8.9
	for i, e in ctx.vb {
		assert e == ctx.vb[i]
	}
	assert ctx.vb == ctx.vb
	/*
	println( ctx.vb[0] )
	println( ctx.vb[1] )
	println( ctx.vb[2] )
	println( ctx.vb[3] )
	*/
}

fn multiply_by_two(mut arr [3]int) {
	for i in 0 .. arr.len {
		arr[i] *= 2
	}
}

fn change_first_element(mut arr [3][3]int) {
	arr[0][0] = 0
}

fn test_fixed_array_can_be_passed_as_mut_arg() {
	mut arr := [1, 2, 3]!
	multiply_by_two(mut arr)
	assert arr == [2, 4, 6]!
	mut arr2 := [[1, 2, 3]!, [4, 5, 6]!, [7, 8, 9]!]!
	change_first_element(mut arr2)
	assert arr2 == [[0, 2, 3]!, [4, 5, 6]!, [7, 8, 9]!]!
}

fn test_iteration_over_fixed_array() {
	mut s := u16(0)
	arr := [u16(3), 2, 17, 23]!
	for v in arr {
		s += v
	}
	assert s == 45
}

fn test_iteration_over_fixed_array_literal() {
	mut s := 0.0
	for v in [0.5, -2.25, 3.75, 12.0, 13.25]! {
		s += v
	}
	assert s == 27.25
}

fn calc_size(a [3]int) {
	mut s := 0
	for i in a {
		println(i)
		s += i
	}
	assert s == 6
}

fn test_for_in_fixed_array() {
	arr := [1, 2, 3]!
	calc_size(arr)
}

//

fn print_fixed_arr(a [2]int) {
	println('a: ${a}')
	assert a == [1, 2]!
	tmp := a
	println('tmp: ${tmp}')
	assert tmp == [1, 2]!
}

fn test_assignment_of_a_fixed_array_passed_as_parameter() {
	arr := [1, 2]!
	assert arr == [1, 2]!
	tmp := arr
	assert tmp == [1, 2]!
	println(tmp) // [1, 2]
	print_fixed_arr(arr)
}
