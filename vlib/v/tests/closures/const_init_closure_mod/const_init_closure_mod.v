module const_init_closure_mod

@[heap]
pub struct Counter {
pub mut:
	n int
}

fn (c &Counter) get() int {
	return c.n
}

fn closure_total(start int) int {
	counter := &Counter{start}
	method_value := counter.get
	doubled := fn [counter] () int {
		return counter.n * 2
	}
	return method_value() + doubled()
}

// initial creates closures while the module's consts are initialized, before
// any code of the program runs; nothing imports the closure runtime explicitly.
pub const initial = closure_total(5)
