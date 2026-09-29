struct Filler {
	value int
}

struct Buffer {
mut:
	data [4]int
}

fn fill_nines(mut a []int) {
	for i in 0 .. a.len {
		a[i] = 9
	}
}

fn fill_generic[T](mut a []T, value T) {
	for i in 0 .. a.len {
		a[i] = value
	}
}

fn (f Filler) fill(mut a []int) {
	for i in 0 .. a.len {
		a[i] = f.value
	}
}

fn append_one(mut a []int) {
	a << 1
	a[0] = 5
}

fn test_mut_slice_of_fixed_array_argument_writes_in_place() {
	mut a := [4]int{}
	fill_nines(mut a[1..3])
	assert a == [0, 9, 9, 0]!
	fill_nines(mut a[..1])
	assert a == [9, 9, 9, 0]!
	mut b := Buffer{}
	fill_nines(mut b.data[2..])
	assert b.data == [0, 0, 9, 9]!
	mut n := [2][3]int{}
	fill_nines(mut n[1][..2])
	assert n == [[0, 0, 0]!, [9, 9, 0]!]!
}

fn test_mut_slice_of_fixed_array_method_and_generic_arguments() {
	mut a := [4]int{}
	Filler{7}.fill(mut a[2..])
	assert a == [0, 0, 7, 7]!
	mut s := ['', '', '']!
	fill_generic(mut s[1..], 'x')
	assert s == ['', 'x', 'x']!
}

fn test_growing_a_mut_slice_of_fixed_array_leaves_it_unchanged() {
	mut a := [4]int{}
	append_one(mut a[..2])
	assert a == [0, 0, 0, 0]!
}

fn test_in_place_methods_on_slice_of_fixed_array() {
	mut a := [4, 3, 2, 1]!
	a[..3].sort()
	assert a == [2, 3, 4, 1]!
	a[1..].reverse_in_place()
	assert a == [2, 1, 4, 3]!
	a[..].sort_with_compare(fn (x &int, y &int) int {
		return *y - *x
	})
	assert a == [4, 3, 2, 1]!
	mut b := Buffer{
		data: [3, 1, 2, 0]!
	}
	b.data[..3].sort()
	assert b.data == [1, 2, 3, 0]!
}

fn test_mut_slice_of_dynamic_array_argument_still_writes_in_place() {
	mut a := [0, 0, 0, 0]
	fill_nines(mut a[1..3])
	assert a == [0, 9, 9, 0]
	a[..2].reverse_in_place()
	assert a == [9, 0, 9, 0]
}
