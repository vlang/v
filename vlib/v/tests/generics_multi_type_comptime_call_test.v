struct Foo {}

['/'; 'GET']
fn (mut f Foo) hello() string {
	return @FN
}

struct Bar {}

['/'; 'GET']
fn (b &Bar) world() string {
	return @FN
}

fn execute_methods<T>() string {
	tmp := T{}
	$for method in T.methods {
		if method.attrs.len >= 2 {
			fun_path := method.attrs[0]
			fun_method := method.attrs[1]

			if fun_path == '/' && fun_method == 'GET' {
				ret := tmp.$method()
				return ret
			}
		}
	}
	return ''
}

fn test_generics_multi_type_comptime_call() {
	ret1 := execute_methods<Foo>()
	println(ret1)
	assert ret1 == 'hello'

	ret2 := execute_methods<Bar>()
	println(ret2)
	assert ret2 == 'world'
}
