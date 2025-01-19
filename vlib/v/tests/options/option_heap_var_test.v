@[heap]
struct Foo {
	a int
}

struct EdgeService {
	foo Foo
}

fn t[S](s S) {
}

fn test_main() {
	mut svc := ?EdgeService(EdgeService{})
	if svc != none {
		t(svc)
		assert true
	} else {
		assert false
	}
}
