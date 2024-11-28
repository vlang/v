struct Foo1 {
	before_1 int
	thr      thread
	after_1  int
	after_2  int
	after_3  int
}

struct Foo2 {
	thr thread
	a   ?int
}

struct Foo3 {
	thrs []thread
	a    []int
}

struct Foo4 {
	thrs []thread int
	a    []int
}

fn test_parse_thread_type() {
	_ = &Foo1{}
	_ = &Foo2{}
	_ = &Foo3{}
	_ = &Foo4{}
	assert true
}
