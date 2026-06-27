// Guards the single element push fast path in `array.push`/`push_noscan`.
// Single element pushes dispatch the common small element sizes (1/2/4/8/16 bytes) to
// `vmemcpy` with a constant size, and fall back to a runtime sized `vmemcpy` otherwise.
// These tests make sure every size copies the right bytes for awkward element types too:
// low alignment size 8/16 structs, structs with padding, and floats - i.e. types where a
// typed integer copy would impose alignment it does not have or read padding through the
// wrong type. They also check that pushing to a slice view clones the backing buffer
// instead of corrupting the parent array.

struct S16 {
	a u64
	b u64
}

struct S3 {
	a u8
	b u8
	c u8
}

struct S24 {
	a u64
	b u64
	c u64
}

// Pair8 is 8 bytes with only 4 byte alignment (no field needs 8 byte alignment).
struct Pair8 {
	a int
	b int
}

// Padded8 is 8 bytes and contains padding: a u8 followed by 3 padding bytes, then a u32.
struct Padded8 {
	a u8
	b u32
}

// LowAlign16 is a 16 byte element with only 1 byte alignment (a fixed byte array field).
// This is the case a 16 byte typed (two u64) copy would mishandle on strict alignment targets.
struct LowAlign16 {
mut:
	data [16]u8
}

// Padded16 is 16 bytes and contains padding: a u8 followed by 3 padding bytes, then three
// u32s. It avoids u64 fields so its size is exactly 16 on every ABI (u64 alignment can be 4
// on some 32-bit targets, which would make a u8+u64 struct 12 bytes instead of 16).
struct Padded16 {
	a u8
	b u32
	c u32
	d u32
}

fn test_push_u8() {
	mut a := []u8{cap: 4}
	for i in 0 .. 300 {
		a << u8(i & 0xff)
	}
	assert a.len == 300
	for i in 0 .. 300 {
		assert a[i] == u8(i & 0xff)
	}
}

fn test_push_u16() {
	mut a := []u16{}
	for i in 0 .. 300 {
		a << u16(i * 7)
	}
	for i in 0 .. 300 {
		assert a[i] == u16(i * 7)
	}
}

fn test_push_int() {
	mut a := []int{}
	for i in 0 .. 300 {
		a << i * 12345
	}
	for i in 0 .. 300 {
		assert a[i] == i * 12345
	}
}

fn test_push_i64() {
	mut a := []i64{}
	for i in 0 .. 300 {
		a << i64(i) * 9876543210
	}
	for i in 0 .. 300 {
		assert a[i] == i64(i) * 9876543210
	}
}

fn test_push_f64() {
	mut a := []f64{}
	for i in 0 .. 100 {
		a << f64(i) + 0.5
	}
	for i in 0 .. 100 {
		assert a[i] == f64(i) + 0.5
	}
}

fn test_push_string() {
	// string is a 16 byte element
	mut a := []string{}
	for i in 0 .. 200 {
		a << 'item_${i}'
	}
	for i in 0 .. 200 {
		assert a[i] == 'item_${i}'
	}
}

fn test_push_struct_16() {
	mut a := []S16{}
	for i in 0 .. 200 {
		a << S16{u64(i), u64(i) * 2}
	}
	for i in 0 .. 200 {
		assert a[i].a == u64(i)
		assert a[i].b == u64(i) * 2
	}
}

fn test_push_struct_odd_size() {
	// 3 byte element exercises the vmemcpy fallback path
	mut a := []S3{}
	for i in 0 .. 200 {
		a << S3{u8(i), u8(i + 1), u8(i + 2)}
	}
	for i in 0 .. 200 {
		assert a[i].a == u8(i)
		assert a[i].b == u8(i + 1)
		assert a[i].c == u8(i + 2)
	}
}

fn test_push_struct_large() {
	// 24 byte element exercises the vmemcpy fallback path
	mut a := []S24{}
	for i in 0 .. 200 {
		a << S24{u64(i), u64(i) * 2, u64(i) * 3}
	}
	for i in 0 .. 200 {
		assert a[i].a == u64(i)
		assert a[i].b == u64(i) * 2
		assert a[i].c == u64(i) * 3
	}
}

fn test_push_to_slice_view_clones() {
	mut a := []int{cap: 10}
	for i in 0 .. 5 {
		a << i
	}
	// a real slice view into a's buffer, with spare capacity after it
	mut s := unsafe { a[1..3] }
	// pushing to a slice view must clone first, not overwrite a[3]
	s << 999
	assert a == [0, 1, 2, 3, 4]
	assert s == [1, 2, 999]
}

fn test_push_to_byte_slice_view_clones() {
	mut a := []u8{cap: 10}
	for i in 0 .. 5 {
		a << u8(i)
	}
	mut s := unsafe { a[1..3] }
	s << u8(200)
	assert a == [u8(0), 1, 2, 3, 4]
	assert s == [u8(1), 2, 200]
}

fn test_push_f32() {
	// f32 is a 4 byte element; the copy must not route it through an integer type
	mut a := []f32{}
	for i in 0 .. 100 {
		a << f32(i) + 0.25
	}
	for i in 0 .. 100 {
		assert a[i] == f32(i) + 0.25
	}
}

fn test_push_pair8_low_alignment() {
	assert sizeof(Pair8) == 8
	mut a := []Pair8{}
	for i in 0 .. 200 {
		a << Pair8{i, i * 3}
	}
	for i in 0 .. 200 {
		assert a[i].a == i
		assert a[i].b == i * 3
	}
}

fn test_push_padded8() {
	assert sizeof(Padded8) == 8
	mut a := []Padded8{}
	for i in 0 .. 200 {
		a << Padded8{u8(i), u32(i) * 7}
	}
	for i in 0 .. 200 {
		assert a[i].a == u8(i)
		assert a[i].b == u32(i) * 7
	}
}

fn test_push_low_alignment_16() {
	assert sizeof(LowAlign16) == 16
	mut a := []LowAlign16{}
	for i in 0 .. 200 {
		mut e := LowAlign16{}
		for j in 0 .. 16 {
			e.data[j] = u8(i + j)
		}
		a << e
	}
	for i in 0 .. 200 {
		for j in 0 .. 16 {
			assert a[i].data[j] == u8(i + j)
		}
	}
}

fn test_push_padded16() {
	assert sizeof(Padded16) == 16
	mut a := []Padded16{}
	for i in 0 .. 200 {
		a << Padded16{u8(i), u32(i) * 99, u32(i) * 7, u32(i) * 13}
	}
	for i in 0 .. 200 {
		assert a[i].a == u8(i)
		assert a[i].b == u32(i) * 99
		assert a[i].c == u32(i) * 7
		assert a[i].d == u32(i) * 13
	}
}
