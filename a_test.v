interface AA {
mut:
	a int
}

struct BB {
mut:
	a int = 3
}

fn test_a() {
	b := BB{}
	assert b.a == 3

	mut a := AA(b) // 1. cannot make mut out unmut
	assert a.a == 3
	a.a = 3
	assert a.a == 4
	assert b.a == 4
}
