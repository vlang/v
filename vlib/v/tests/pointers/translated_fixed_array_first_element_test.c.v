// c2v lowers C array-to-pointer decay to `unsafe { &array[0] }`, so translated
// pointer arithmetic over fixed arrays needs no implicit decay in V.
@[has_globals; translated]
module main

fn C.strlen(&i8) usize

__global c2v_buf [16]i8
__global c2v_buf_offset = unsafe { &c2v_buf[0] } + 3

struct C2vHolder {
	vals [4]int
	name [8]i8
}

__global c2v_holder C2vHolder
__global c2v_field_offset = unsafe { &c2v_holder.vals[0] } + 2

fn c2v_copy(out &i8, in_ &i8, num usize) &i8 {
	C.memcpy(voidptr(out), voidptr(in_), num)
	out[num] = 0
	return out
}

fn c2v_offsets(s &C2vHolder, n int, flag int) int {
	r := unsafe { &s.vals[0] } + n
	t := if flag != 0 { unsafe { &s.vals[0] } + 1 } else { unsafe { &s.vals[0] } + 2 }
	u := unsafe { &s.name[0] } + (if flag != 0 { 1 } else { 2 })
	q := 2 + unsafe { &c2v_buf[0] }
	c2v_copy(unsafe { &i8(&c2v_buf[0]) }, c'hello', if flag != 0 { 3 } else { 4 })
	e := unsafe { &c2v_buf[0] } + C.strlen(unsafe { &i8(&c2v_buf[0]) })
	k := int(i64((isize(e) - isize(unsafe { &c2v_buf[0] })) / isize(sizeof(i8))))
	assert k == if flag != 0 { 3 } else { 4 }
	assert unsafe { *q } == `l`
	return int(unsafe { *r } + unsafe { *t }) +
		int(i64((isize(u) - isize(unsafe { &s.name[0] })) / isize(sizeof(i8))))
}

fn test_translated_first_element_arithmetic() {
	s := C2vHolder{
		vals: [1, 2, 3, 4]!
	}
	assert c2v_offsets(&s, 2, 1) == 3 + 2 + 1
	assert c2v_offsets(&s, 0, 0) == 1 + 3 + 2
}

fn test_translated_global_initializers() {
	c2v_buf[3] = 9
	assert unsafe { *c2v_buf_offset } == 9
	c2v_holder.vals[2] = 5
	assert unsafe { *c2v_field_offset } == 5
	d := i64((isize(c2v_field_offset) - isize(unsafe { &c2v_holder.vals[0] })) / isize(sizeof(int)))
	assert d == 2
}
