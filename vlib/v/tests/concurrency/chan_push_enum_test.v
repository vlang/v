enum FooBar {
	foo
	bar
}

fn test_chan_push_enum() {
	ch := chan FooBar{cap: 10}
	ch <- .foo
	println(<-ch)
	assert true
}
