enum Foo {
	a
	b
	c
	d
	e
	f
}

fn test() string {
	return 'c'
}

fn dummy() string {
	return 'b'
}

fn dummy2() string {
	return 'a'
}

fn dummy3() {}

fn err() fn () string {
	return match Foo.a {
		.a { dummy }
		.b { dummy }
		.c { dummy }
		.d { dummy2 }
		.e { dummy2 }
		else { dummy2 }
	}
}

fn err2() fn () string {
	return match Foo.d {
		.a { dummy }
		.b { dummy }
		.c { dummy }
		.d { dummy2 }
		.e { dummy2 }
		.f { dummy }
	}
}

fn err3() fn () string {
	return match Foo.f {
		.a { dummy }
		.b { dummy }
		.c { dummy }
		.d { dummy }
		.e { dummy }
		else { test }
	}
}

fn err4() fn () {
	return match Foo.f {
		.a { dummy3 }
		.b { dummy3 }
		.c { dummy3 }
		.d { dummy3 }
		.e { dummy3 }
		else { dummy3 }
	}
}

fn err5(f fn ()) fn () {
	return match Foo.f {
		.a { dummy3 }
		.b { dummy3 }
		.c { dummy3 }
		.d { dummy3 }
		.e { dummy3 }
		else { f }
	}
}

fn test_main() {
	var := err()()
	assert var == 'b'

	var2 := err2()()
	assert var2 == 'a'

	var3 := err3()()
	assert var3 == 'c'

	var4 := err4()
	assert var4 == dummy3

	anon := fn () {}
	var5 := err5(anon)
	assert var5 == anon
	var5()
}
