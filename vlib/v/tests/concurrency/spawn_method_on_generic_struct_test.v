struct Gen[T] {
	data T
}

fn (g Gen[T]) process() string {
	th := spawn g.internal()
	r := th.wait()
	return r
}

fn (g Gen[T]) internal() string {
	println(g.data * g.data)
	return '${g.data * g.data}'
}

fn test_spawn_method_on_generic_struct() {
	g := Gen[int]{
		data: 5
	}
	r1 := g.process()
	assert r1 == '25'

	r2 := g.internal()
	assert r2 == '25'
}
