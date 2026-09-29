struct Keeper {
mut:
	data []int
}

fn (mut k Keeper) keep(mut a []int) {
	k.data = a
}

fn pass_through(mut a []int) []int {
	return a
}

fn returned_whole() []int {
	mut x := [1, 2, 3]!
	return pass_through(mut x)
}

fn returned_range() []int {
	mut x := [1, 2, 3]!
	return pass_through(mut x[1..])
}

fn stored_whole(mut k Keeper) {
	mut x := [4, 5, 6]!
	k.keep(mut x)
}

// overwrite_stack reuses the stack that a view into a dead fixed array would point to.
fn overwrite_stack() int {
	b := [7, 7, 7, 7, 7, 7, 7, 7]!
	return b[3]
}

fn fill(mut a []int, value int) {
	for i in 0 .. a.len {
		a[i] = value
	}
}

fn append_then_write(mut a []int) {
	a[0] = 1
	a << 5
	a[1] = 2
}

fn reassign(mut a []int) {
	a[0] = 4
	a = [7, 7, 7]
	a[1] = 8
}

fn write_then_fail(mut a []int) !int {
	a[0] = 5
	return error('failed')
}

fn forward_failure(mut out [2]int) !int {
	return write_then_fail(mut out)!
}

fn test_returned_mut_fixed_array_argument_outlives_the_array() {
	whole := returned_whole()
	assert overwrite_stack() == 7
	assert whole == [1, 2, 3]
	ranged := returned_range()
	assert overwrite_stack() == 7
	assert ranged == [2, 3]
}

fn test_stored_mut_fixed_array_argument_outlives_the_array() {
	mut k := Keeper{}
	stored_whole(mut k)
	assert overwrite_stack() == 7
	assert k.data == [4, 5, 6]
}

fn test_mut_fixed_array_argument_still_writes_in_place() {
	mut a := [0, 0, 0]!
	fill(mut a, 9)
	assert a == [9, 9, 9]!
	fill(mut a[1..], 3)
	assert a == [9, 3, 3]!
	mut b := [0, 0, 0]!
	append_then_write(mut b)
	assert b == [1, 0, 0]!
	mut c := [0, 0, 0]!
	reassign(mut c)
	assert c == [4, 0, 0]!
	mut d := [0, 0]!
	forward_failure(mut d) or { assert err.msg() == 'failed' }
	assert d == [5, 0]!
}
