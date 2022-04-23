module main

fn test_mark_as_referenced() {
	if true {
		a := Type{}
		ret := f(a)
		println(ret)
		assert ret == 'Interface(Type{})'
	}
	a := Type{}
	ret := f(a)
	println(ret)
	assert ret == 'Interface(Type{})'
}

struct Type {
}

interface Interface {
}

fn f(b Interface) string {
	return '$b'
}
