interface IAbc {
	name string
	xyz()
}

fn f(i &IAbc) string {
	return '${i}'
}

struct Abc {
	name string
	x    int = 123
}

fn (a Abc) xyz() {}

fn resource__null() &IAbc {
	return unsafe { nil }
}

fn test_fn_returning_voidptr_casted_as_interface_works() {
	pi := resource__null()
	// TODO: understand the root reason why msvc and
	// `-cc clang-11 -cflags -fsanitize=memory` produce
	// something like `&IAbc(e42aff650)` here
	assert f(pi).contains('&IAbc(')
}
