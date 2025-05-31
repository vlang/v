type SumType = AA | BB

struct AA {}

struct BB {}

fn (a &AA) foo() int {
	println('AA.foo()')
	return 200
}

fn (b &BB) foo() int {
	println('BB.foo()')
	return 100
}

fn test_main() {
	x := SumType(BB{})
	match x {
		AA, BB {
			ret := x.foo()
			assert ret == 100
			return
		}
	}
	assert false
}
