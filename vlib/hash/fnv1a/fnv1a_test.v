import hash.fnv1a

struct MyStruct {
mut:
	x int
	y int
	z int
}

struct Abc {
mut:
	a [5]u64
}

fn test_fnv1a_sum32() {
	$if windows {
		return
	}
	ahash := '10bc2abf'
	a := 'apple'
	b := fnv1a.sum32_string(a)
	c := fnv1a.sum32(a.bytes())
	d := unsafe { fnv1a.sum32_bytes(a.str, a.len) }
	assert b.hex() == ahash
	assert c.hex() == ahash
	assert d.hex() == ahash

	mut aa := Abc{}
	x := fnv1a.sum32_struct(aa)
	aa.a[3] = 5
	y := fnv1a.sum32_struct(aa)
	assert x != y
	mut ms := MyStruct{}
	xx := fnv1a.sum32_struct(ms)
	ms.x = 77
	yy := fnv1a.sum32_struct(ms)
	assert xx != yy
	assert x != xx
	assert y != yy
}

fn test_fnv1a_sum64() {
	$if windows {
		return
	}
	a := 'apple'
	ahash := 'f74a62a458befdbf'
	b := fnv1a.sum64_string(a)
	c := fnv1a.sum64(a.bytes())
	d := unsafe { fnv1a.sum64_bytes(a.str, a.len) }
	assert b.hex() == ahash
	assert c.hex() == ahash
	assert d.hex() == ahash

	mut aa := Abc{}
	x := fnv1a.sum64_struct(aa)
	aa.a[3] = 5
	y := fnv1a.sum64_struct(aa)
	assert x != y
	mut ms := MyStruct{}
	xx := fnv1a.sum64_struct(ms)
	ms.x = 77
	yy := fnv1a.sum64_struct(ms)
	assert xx != yy
	assert x != xx
	assert y != yy
}
