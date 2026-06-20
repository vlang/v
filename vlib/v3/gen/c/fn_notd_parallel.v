module c

fn (mut g FlatGen) gen_fns_dispatch(_no_parallel bool) {
	g.gen_fns()
}
