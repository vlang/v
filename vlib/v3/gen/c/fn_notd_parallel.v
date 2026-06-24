module c

// gen_fns_dispatch emits fns dispatch output for c.
fn (mut g FlatGen) gen_fns_dispatch(_no_parallel bool) {
	g.gen_fns()
}
