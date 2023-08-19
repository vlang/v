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
	thread_str := spawn a.sub.get()
	r := thread_str.wait()
	assert r == 'hi'
}

fn (t SubTest) get() string {
	return t.test
}
