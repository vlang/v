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

struct Counter {
mut:
	n int
}

fn (mut c Counter) bump() {
	c.n++
}

fn chained_fixed_slice() []int {
	a := [1, 2, 3]!
	return a[..][1..]
}

// overwrite_stack reuses the stack that a returned view into a dead fixed array would use.
fn overwrite_stack() int {
	b := [7, 7, 7, 7, 7, 7, 7, 7]!
	return b[3]
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

fn test_parenthesized_fixed_array_slice_sort() {
	mut a := [3, 2, 1]!
	(a[..]).sort()
	assert a == [1, 2, 3]!
}

fn test_parenthesized_slices_of_fixed_array_write_in_place() {
	mut a := [1, 2, 3]!
	(a[1..]).reverse_in_place()
	assert a == [1, 3, 2]!
	mut b := [0, 0, 0]!
	fill_nines(mut (b[1..]))
	assert b == [0, 9, 9]!
}

fn test_mut_slice_of_dynamic_array_argument_still_writes_in_place() {
	mut a := [0, 0, 0, 0]
	fill_nines(mut a[1..3])
	assert a == [0, 9, 9, 0]
	a[..2].reverse_in_place()
	assert a == [9, 0, 9, 0]
}

fn test_for_mut_over_slice_of_fixed_array_writes_in_place() {
	mut a := [1, 2, 3, 4]!
	for mut x in a[1..] {
		x = 9
	}
	assert a == [1, 9, 9, 9]!
	for i, mut x in a[..2] {
		x = i + 10
	}
	assert a == [10, 11, 9, 9]!
	mut b := Buffer{}
	for mut x in b.data[2..] {
		x = 5
	}
	assert b.data == [0, 0, 5, 5]!
	mut n := [2][3]int{}
	for mut x in n[1][..2] {
		x = 6
	}
	assert n == [[0, 0, 0]!, [6, 6, 0]!]!
}

fn test_assigning_through_slice_of_fixed_array_writes_in_place() {
	mut a := [1, 2, 3]!
	a[1..][0] = 8
	assert a == [1, 8, 3]!
	a[1..][1] += 10
	assert a == [1, 8, 13]!
	(a[..2])[0] = 0
	assert a == [0, 8, 13]!
	assert a[1..][0] == 8
	mut b := Buffer{}
	b.data[2..][1] = 4
	assert b.data == [0, 0, 0, 4]!
	mut n := [2][3]int{}
	n[1][1..][0] = 6
	assert n == [[0, 0, 0]!, [0, 6, 0]!]!
	mut s := ['a', 'b']!
	s[1..][0] = 'z'
	assert s == ['a', 'z']!
}

fn test_element_of_array_slice_is_mutated_in_place() {
	mut fixed := [Counter{1}, Counter{2}, Counter{3}]!
	fixed[1..][0].n = 20
	assert fixed[1].n == 20
	fixed[1..][1].bump()
	assert fixed[2].n == 4
	mut dynamic := [Counter{1}, Counter{2}, Counter{3}]
	dynamic[1..][1].bump()
	assert dynamic[2].n == 4
}

fn test_chained_fixed_slice_keeps_copy_semantics() {
	mut a := [1, 2, 3]!
	snapshot := a[..][..]
	a[0] = 99
	assert snapshot == [1, 2, 3]
	tail := a[1..][..]
	a[2] = 42
	assert tail == [2, 3]
	assert a[..][2] == 42
}

fn test_chained_fixed_slice_returned_from_a_helper_is_independent() {
	values := chained_fixed_slice()
	assert overwrite_stack() == 7
	assert values == [2, 3]
}
