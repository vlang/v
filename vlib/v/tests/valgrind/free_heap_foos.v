[has_globals]
module main

__global frees = []int{cap: 100}

struct Foo {
	x int
}

fn create(x int) &Foo {
	res := &Foo{x}
	println('> creating Foo $res.x at address: ${voidptr(res)}')
	return res
}

fn (f &Foo) free() {
	println('> freeing Foo $f.x at address: ${voidptr(f)} | frees.len: $frees.len')
	frees << f.x
}

fn create_some_foos() {
	starting := frees.len
	a := create(111)
	assert frees.len == starting
	b := create(222)
	assert frees.len == starting
	println('  > create_some_foos a: $a.x')
	println('  > create_some_foos b: $b.x')
	assert frees.len == starting
}

fn main() {
	create_some_foos()
	if C._VAUTOFREE == 1 {
		assert frees.len == 2
		assert frees[0] == 222
		assert frees[1] == 111
	}
	create_some_foos()
	if C._VAUTOFREE == 1 {
		assert frees.len == 4
		assert frees[0] == 222
		assert frees[1] == 111
		assert frees[2] == 222
		assert frees[3] == 111
	}
	unsafe { frees.free() }
}
