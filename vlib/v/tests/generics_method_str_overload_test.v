struct Gen<G> {
	data G
	id   int
	size int
}

fn (g Gen<G>) str() string {
	return 'Gen<G> $g.id, $g.data, ${g.size}.'
}

fn test_generics_method_str_overload() {
	mut g := Gen<string>{'Gen', 0, 10}
	println(g)
	assert '$g' == 'Gen<G> 0, Gen, 10.'
}
