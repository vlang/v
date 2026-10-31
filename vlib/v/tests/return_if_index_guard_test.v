// An indexing or receiving guard in a returned `if` tests the index or the channel, as it
// does in a guard statement, and its binding can leave the function by address.
fn element_or_default(items []int, i int) int {
	return if x := items[i] { x + 1 } else { -1 }
}

fn element_address(items []int, i int) &int {
	return if mut x := items[i] {
		p := &x
		x += 100
		p
	} else {
		&int(unsafe { nil })
	}
}

fn element_in_else_if(items []int, i int) int {
	return if i < 0 {
		-2
	} else if x := items[i] {
		x * 2
	} else {
		-1
	}
}

fn received_or_default(ch chan int) int {
	return if x := <-ch { x + 1 } else { -1 }
}

@[noinline]
fn overwrite_stack() int {
	mut filler := [64]int{}
	for i in 0 .. filler.len {
		filler[i] = 0x55555555
	}
	return filler[7]
}

fn test_array_index_guard_in_returned_if() {
	items := [10, 20, 30]
	assert element_or_default(items, 1) == 21
	assert element_or_default(items, 3) == -1
	assert element_or_default([]int{}, 0) == -1
}

fn test_array_index_guard_binding_address() {
	items := [10, 20, 30]
	p := element_address(items, 2)
	overwrite_stack()
	assert *p == 130
	assert items[2] == 30
	assert isnil(element_address(items, 5))
}

fn test_array_index_guard_in_else_if_of_returned_if() {
	items := [10, 20, 30]
	assert element_in_else_if(items, -1) == -2
	assert element_in_else_if(items, 0) == 20
	assert element_in_else_if(items, 9) == -1
}

fn test_channel_guard_in_returned_if() {
	ch := chan int{cap: 1}
	ch <- 41
	assert received_or_default(ch) == 42
	ch.close()
	assert received_or_default(ch) == -1
}
