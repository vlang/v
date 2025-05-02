module main

fn test_a() {
	struct Ak {
		a int
		b int
	}

	cases := [
		Ak{1, 1},
		Ak{2, 2},
	]
	for _, k in cases {
		assert k.a == k.b
	}
}

fn test_b() {
	struct Ak {
		a int
		b int
		c int
	}

	cases := [
		Ak{1, 2, 2},
		Ak{2, 2, 2},
	]
	for _, k in cases {
		assert k.b == k.c
	}
}
