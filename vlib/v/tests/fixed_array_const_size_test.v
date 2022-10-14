const (
	size     = 5
	u64_size = u64(5)
)

struct Foo {
	bar [size]u8
}

fn test_fixed_array_const_size() {
	a := Foo{}
	println(a)
	assert a == Foo{
		bar: [u8(0), 0, 0, 0, 0]!
	}
}

fn test_fixed_array_const_u64_size() {
	a := [2 * u64_size]f64{}
	println(a)
	assert '$a' == '[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]'
}

const n = u64(5_000)

const nn = 5_000

fn test_u64_const_used_as_fixed_array_size() {
	mut a := [2 * n]f64{}
	dump(a.len)
	assert a.len == 10000

	mut b := [n * 2]f64{}
	dump(b.len)
	assert b.len == 10000
}

fn test_int_const_used_as_fixed_array_size() {
	mut aa := [2 * nn]f64{}
	dump(aa.len)
	assert aa.len == 10_000

	mut bb := [nn * 2]f64{}
	dump(bb.len)
	assert aa.len == 10_000
}
