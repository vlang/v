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
	t := spawn a.sub.get()
	r := t.wait()
	assert r == 'hi'
}

fn (t SubTest) get() string {
	return t.test
}
