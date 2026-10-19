import params

fn test_needs_a_map() {
	x := params.needs_a_map({
		'key': 1
	})
	assert x == 1
}
