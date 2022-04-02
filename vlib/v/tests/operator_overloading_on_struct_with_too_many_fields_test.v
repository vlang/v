// NB: the struct here deliberately has 9 member fields, to trigger
// the V heuristic, that decides to pass a Foo receiver automatically
// by reference, when a struct is too large.
struct Foo {
	one   int
	two   int
	three int
	four  int
	five  int
	six   int
	seven int
	eight int
	nine  int
}

fn (_ Foo) == (_ Foo) bool {
	return true
}

fn test_op() {
	a := Foo{
		one: 1
	}
	b := Foo{
		two: 2
	}
	assert a == b
	dump(a)
	dump(b)
}
