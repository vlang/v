@[heap]
struct Demo {
	a string
}

fn test_main() {
	mut rl := [&Demo{'A'}]

	assert rl[0].a == 'A'

	mut p := &Demo{}
	for mut e in rl {
		p = e
	}

	assert p.a == 'A'

	for i in 0 .. rl.len {
		mut e := rl[i]
		p = e
	}

	assert p.a == 'A'
}
