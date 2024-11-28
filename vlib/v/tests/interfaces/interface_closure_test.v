interface ITest {
mut:
	caller(a Test) !
}

struct Test {
}

struct Test2 {
}

fn (t2 Test2) with_reader(func fn (a Test) !) ! {
	return func(Test{})
}

fn (t Test) caller(a Test) ! {
	println('ok')
}

fn get() ITest {
	return Test{}
}

fn test_main() {
	mut a := get()

	b := Test2{}
	b.with_reader(a.caller)!
	assert true
}
