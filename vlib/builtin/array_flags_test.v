fn show_array(name string, a []int) {
	eprintln('${name:10} .flags: ${a.flags:34} | .cap: ${a.cap:2} | .len: ${a.len:2} | .data: ${a.data} | ${a}')
}

fn trace_delete_elements(name string, mut a []int) int {
	a.delete_many(5, 3)
	show_array(name, a)
	a << 55
	show_array(name, a)
	a << 66
	show_array(name, a)
	a << 77
	res := a.cap
	eprintln('                                        <<   ${name:10} .cap: ${a.cap} >>')
	show_array(name, a)
	a << 88
	show_array(name, a)
	a << 99
	show_array(name, a)
	eprintln('-------------------------------')
	return res
}

fn test_array_cap_shrinkage_after_deletion() {
	mut a := [0]
	mut middle_cap := 0

	a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	middle_cap = trace_delete_elements('normal', mut a)
	assert middle_cap == 14
	assert a.len == 12
	assert a.cap == 14

	a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	unsafe { a.flags.set(.noslices) }
	middle_cap = dump(trace_delete_elements('noslices', mut a))
	assert middle_cap == 14
	assert a.len == 12
	assert a.cap == 14

	a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	unsafe { a.flags.set(.noshrink) }
	middle_cap = dump(trace_delete_elements('noshrink', mut a))
	assert middle_cap == 14
	assert a.len == 12
	assert a.cap == 14

	// Note: when *both* flags are set, the memory block for the array
	// should NOT shrink on deleting array elements, thus << after the
	// deletion, will still have space (till .cap is reached).
	a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	unsafe { a.flags.set(.noslices | .noshrink) }
	middle_cap = dump(trace_delete_elements('both', mut a))
	assert middle_cap == 10
	assert a.len == 12
	assert a.cap == 20
}

fn fixed_array_on_the_heap(len int, size int) []u8 {
	data := vcalloc(size)
	println(ptr_str(data))
	return unsafe {
		array{
			element_size: 1
			len:          len
			cap:          size
			data:         data
			flags:        .noshrink | .nogrow | .nofree
		}
	}
}

fn test_array_fixed_growth() {
	mut x := fixed_array_on_the_heap(0, 10)
	println(ptr_str(x.data))
	x << 5
	x << 10
	x << 15
	x << 20
	dump(x)
	dump(x.flags)
	assert x[2] == 15
	assert x.flags == .noshrink | .nogrow | .nofree
}

fn test_array_fixed() {
	mut x := fixed_array_on_the_heap(10, 10)
	println(ptr_str(x.data))
	x[2] = 5
	dump(x)
	dump(x.flags)
	assert x[2] == 5
	assert x.flags == .noshrink | .nogrow | .nofree
}
