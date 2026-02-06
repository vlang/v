struct None {}

type Myfn = fn (int) int

type Myfnfact = fn () Myfn

type Maybefnfact = Myfnfact | None

// Myfn
fn abc(i int) int {
	return i
}

fn run(mmff Maybefnfact) string {
	match mmff {
		Myfnfact { return 'yes fn' }
		None { return 'None fn' }
	}
}

fn test_sumtype_with_alias_fntype() {
	// create Myfn
	myfnfact := fn () Myfn {
		return abc
	}
	r1 := myfnfact()(1)
	println(r1)
	assert r1 == 1

	r2 := run(None{})
	println(r2)
	assert r2 == 'None fn'

	r3 := run(myfnfact)
	println(r3)
	assert r3 == 'yes fn'
}
