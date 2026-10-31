// Fixed arrays do not decay to pointers. Pointer arithmetic over them takes the
// address of an element (or casts the array address) inside `unsafe`.
@[has_globals]
module main

__global fixed_ptr_buf [16]u8
__global fixed_ptr_buf_offset = unsafe { &fixed_ptr_buf[0] + 3 }

struct FixedPtrHolder {
mut:
	vals [4]int
}

__global fixed_ptr_holder FixedPtrHolder
__global fixed_ptr_field_offset = unsafe { &fixed_ptr_holder.vals[0] + 2 }

fn fixed_ptr_read(p &int) int {
	return unsafe { *p }
}

fn fixed_ptr_make() [3]int {
	return [7, 11, 13]!
}

fn test_first_element_offsets() {
	values := [3, 5, 7]!
	start := unsafe { &values[0] }
	next := unsafe { &values[0] + 1 }
	assert unsafe { *next } == 5
	assert fixed_ptr_read(unsafe { &values[0] + 2 }) == 7
	assert fixed_ptr_read(unsafe { next - 1 }) == 3
	assert unsafe { 2 + &values[0] } == unsafe { &values[2] }
	assert unsafe { next - start } == 1
	assert unsafe { start - next } == -1
}

fn test_array_address_casts() {
	values := [3, 5, 7]!
	p := unsafe { &int(&values) + 1 }
	assert unsafe { *p } == 5
	b := unsafe { &u8(&values) + sizeof(int) }
	assert unsafe { &int(b) } == p
}

fn test_runtime_offsets() {
	values := [3, 5, 7]!
	n := 2
	flag := true
	assert unsafe { *(&values[0] + n) } == 7
	assert unsafe { *(&values[0] + u64(1)) } == 5
	assert unsafe { *(&values[0] + (if flag { 1 } else { 2 })) } == 5
}

fn test_writes_reach_the_array() {
	mut values := [3, 5, 7]!
	p := unsafe { &values[0] + 1 }
	unsafe {
		*p = 42
	}
	assert values[1] == 42
	mut holder := FixedPtrHolder{}
	field := unsafe { &holder.vals[0] + 3 }
	unsafe {
		*field = 9
	}
	assert holder.vals[3] == 9
}

fn test_nested_arrays() {
	m := [[1, 2, 3]!, [4, 5, 6]!]!
	assert unsafe { *(&m[1][0] + 2) } == 6
	row := unsafe { &m[0] + 1 }
	assert unsafe { (*row)[0] } == 4
}

fn test_returned_array_through_a_variable() {
	values := fixed_ptr_make()
	assert unsafe { *(&values[0] + 1) } == 11
}

fn test_compound_assignment() {
	values := [3, 5, 7]!
	mut p := unsafe { &values[0] }
	unsafe {
		p += 2
	}
	assert unsafe { *p } == 7
	unsafe {
		p--
	}
	assert unsafe { *p } == 5
}

fn test_global_initializers() {
	fixed_ptr_buf[3] = 77
	assert unsafe { *fixed_ptr_buf_offset } == 77
	assert unsafe { fixed_ptr_buf_offset - &fixed_ptr_buf[0] } == 3
	fixed_ptr_holder.vals[2] = 5
	assert unsafe { *fixed_ptr_field_offset } == 5
}
