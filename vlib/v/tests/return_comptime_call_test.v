struct App {}

fn (mut app App) method_one() string {
	return '1'
}

fn (mut app App) method_two() string {
	return '2'
}

fn reflect_call(method_name string) string {
	a := App{}
	$for method in App.methods {
		if method.name == method_name {
			return a.$method()
		}
	}
	panic('Method not supported: ${method_name}')
}

fn test_reflect_call_returning_a_comptime_method() {
	res1 := dump(reflect_call('method_one'))
	assert res1 == '1'
	res2 := dump(reflect_call('method_two'))
	assert res2 == '2'
}
