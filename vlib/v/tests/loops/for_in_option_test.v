import os

fn test_for_in_option() {
	for d in os.read_lines(@FILE) or { panic('not found') } {
		println(d)
	}
	assert true
}

// for issue 20528
// phenomenon: when the cond expr is SelectorExpr and the type is an array, cgen fails.
struct Foo {
	data ?[]int
}

fn (f Foo) get_first() ?int {
	for d in f.data or { return none } {
		return d
	}
	return none
}

fn test_cond_is_selector_array_and_with_or_block() {
	foo := Foo{
		data: [1, 2, 3]
	}
	assert foo.get_first()? == 1
}
