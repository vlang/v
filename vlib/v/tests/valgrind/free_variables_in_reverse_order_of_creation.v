[has_globals]
module main

__global frees = []int{cap: 100}

struct Test {
	a int
}

fn (t &Test) free() {
	frees << t.a
}

fn test(a int) Test {
	return Test{a}
}

fn use_some_returned_variables() {
	a := test(1)
	println(a)
	{
		cc := test(333)
		dd := test(444)
		println(cc)
		println(dd)
	}
	b := test(2)
	if C._VAUTOFREE == 1 {
		assert frees.len == 2
		assert frees[0] == 444
		assert frees[1] == 333
	}
	println(b)
}

fn main() {
	use_some_returned_variables()
	if C._VAUTOFREE == 1 {
		assert frees.len == 4
		assert frees[0] == 444
		assert frees[1] == 333
		assert frees[2] == 2
		assert frees[3] == 1
	}
	unsafe { frees.free() }
}
