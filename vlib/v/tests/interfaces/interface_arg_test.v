interface ITest {
}

struct Test {
}

struct Cmdable {
mut:
	call fn (cmd ITest)
}

fn (t Test) test(a ITest) {}

fn test(a ITest) {}

fn get() &Test {
	return &Test{}
}

fn test_main() {
	mut a := Cmdable{}
	a.call = test
	a.call(Test{})
	test(Test{})
	Test{}.test(a)

	assert true
}

fn test_ptr() {
	mut a := Cmdable{}
	a.call = test
	a.call(get())

	assert true
}
