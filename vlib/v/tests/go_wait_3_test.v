struct Test {
	sub SubTest
}

struct SubTest {
	test string
}

fn test_method_go_wait() {
	a := Test{
		sub: SubTest{
			test: 'hi'
		}
	}
	thread := go a.sub.get()
	r := thread.wait()
	assert r == 'hi'
}

fn (t SubTest) get() string {
	return t.test
}
