struct AB {
	a int
	b int
}

fn (ab AB) value() int {
	return ab.a + ab.b
}

fn test_main() {
	mut values := [AB{5, 6}, AB{3, 4}, AB{1, 2}]
	values.sort(a.value() < b.value())
	assert values[0] == AB{1, 2}
	assert values[1] == AB{3, 4}
	assert values[2] == AB{5, 6}
}
