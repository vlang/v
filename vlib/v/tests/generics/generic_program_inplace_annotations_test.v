// A program that uses generics transforms its function bodies in parallel on
// private copies of the AST. Annotations written in place on existing nodes there
// (the borrow of a bound `mut` receiver, the type of a local read by `defer(fn)`)
// must still reach code generation.
struct Counter {
mut:
	n int
}

fn (mut c Counter) bump(by int) {
	c.n += by
}

fn identity[T](x T) T {
	return x
}

fn test_mut_method_value_updates_receiver() {
	mut c := Counter{}
	bump := unsafe { c.bump }
	bump(identity(2))
	assert c.n == 2
	bump(3)
	assert c.n == 5
}

fn test_fn_defer_reads_inner_block_local() {
	mut x := 0
	defer {
		assert x == 1
	}
	{
		a := identity(1)
		defer(fn) {
			x = a
		}
	}
}

fn test_mut_method_value_on_reference() {
	mut c := &Counter{}
	bump := unsafe { c.bump }
	bump(identity(4))
	assert c.n == 4
}
