struct Counter {
mut:
	n int
}

fn (mut c Counter) bump() {
	c.n++
}

struct Single {
	Counter
}

struct Nested {
	Single
}

fn test_for_mut_calls_mut_methods_of_embedded_structs() {
	mut singles := [Single{Counter{1}}, Single{Counter{5}}]
	for mut item in singles {
		item.bump()
	}
	assert singles.map(it.n) == [2, 6]
	mut nested := [Nested{Single{Counter{10}}}]
	for mut item in nested {
		item.bump()
		item.bump()
	}
	assert nested[0].n == 12
}
