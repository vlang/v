struct Gen<G> {
	data G
	id   int
	size int
}

fn (g Gen<G>) str() string {
	return 'Gen<${G.name}>{${g.id}, ${g.data}, ${g.size}}'
}

fn test_generics_method_str_overload() {
	mut g1 := Gen<string>{'aaa', 0, 10}
	println(g1)
	assert '${g1}' == 'Gen<string>{0, aaa, 10}'

	mut g2 := Gen<int>{22, 0, 10}
	println(g2)
	assert '${g2}' == 'Gen<int>{0, 22, 10}'

	mut g3 := Gen<f64>{2.22, 0, 10}
	println(g3)
	assert '${g3}' == 'Gen<f64>{0, 2.22, 10}'

	mut g4 := Gen<bool>{true, 0, 10}
	println(g4)
	assert '${g4}' == 'Gen<bool>{0, true, 10}'
}
