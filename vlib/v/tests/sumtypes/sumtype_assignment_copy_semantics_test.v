struct Aa {
mut:
	a int
}

struct Bb {
mut:
	a int
}

type Sum = Aa | Bb

fn test_sumtype_assignment_copies_wrapped_value() {
	a := Sum(Aa{
		a: 32
	})
	mut b := a
	b.a = 47
	assert a.a == 32
	assert b.a == 47

	mut c := Sum(Bb{
		a: 1
	})
	c = b
	c.a = 99
	assert b.a == 47
	assert c.a == 99
}
