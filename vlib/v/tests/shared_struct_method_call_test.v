module main

struct Aa {
mut:
	b []int
}

fn append_ok(shared a Aa, new_b int) {
	lock a {
		a.b << new_b
	}
}

fn (shared a Aa) append_fails(new_b int) {
	lock a {
		a.b << new_b
	}
}

fn test_shared_struct_method_call() {
	shared a := Aa{}
	append_ok(shared a, 1)
	a.append_fails(2)
	rlock a {
		println(a.b)
		assert a.b == [1, 2]
	}
}
