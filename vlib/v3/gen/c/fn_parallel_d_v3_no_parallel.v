module c

// gen_fns_dispatch emits all functions serially when v3 is built with the
// internal `v3_no_parallel` define.
fn (mut g FlatGen) gen_fns_dispatch(_ bool) {
	g.gen_fns()
	g.gen_synthetic_main_after_fns()
}
