interface IAbc {
	name string
	xyz()
}

fn f(i &IAbc) string {
	return '$i'
}

struct Abc {
	name string
	x    int = 123
}

fn (a Abc) xyz() {}

fn resource__null() &IAbc {
	return voidptr(0)
}

fn test_fn_returning_voidptr_casted_as_interface_works() {
	pi := resource__null()
	assert f(pi) == '&IAbc(0)'
}
