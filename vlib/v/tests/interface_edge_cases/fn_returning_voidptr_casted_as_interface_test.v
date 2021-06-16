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
	$if msvc {
		// TODO: understand the root reason why msvc produces something like `&IAbc(e42aff650)` here
		assert f(pi).contains('&IAbc(')
	} $else {
		assert f(pi) == '&IAbc(0)'
	}
}
