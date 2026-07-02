module c

// gen_fns_dispatch emits all functions serially when the compiler is built
// without parallel support.
fn (mut g FlatGen) gen_fns_dispatch(_ bool) {
	g.gen_fns()
	g.gen_synthetic_main_after_fns()
}
