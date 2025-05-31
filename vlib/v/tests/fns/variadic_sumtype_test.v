type Either = string | int

fn tag(params ...Either) int {
	mut c := 0
	for p in params {
		if p is int {
			c += p
		}
	}
	return c
}

fn div(params ...Either) int {
	return tag(params)
}

fn test_main() {
	assert dump(div('foo', 1, 2)) == 3
}
