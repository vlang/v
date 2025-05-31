interface TheInterfaceItself {
	f() int
}

struct SomeImplForTII1 {}

fn (_ SomeImplForTII1) f() int {
	return 2
}

fn (_ SomeImplForTII1) secret1() int {
	return 42
}

struct AnotherImplementation {}

fn (_ AnotherImplementation) f() int {
	return 8
}

fn (_ AnotherImplementation) secret2() int {
	return 84
}

fn h(foo TheInterfaceItself, i int) int {
	j := foo.f()
	match i {
		0 {
			return j + (foo as SomeImplForTII1).secret1()
		}
		1 {
			return j + (foo as AnotherImplementation).secret2()
		}
		else {
			return 1
		}
	}
}

fn test_casting_to_impl() {
	assert h(SomeImplForTII1{}, 0) == 44
	assert h(AnotherImplementation{}, 1) == 92
}
