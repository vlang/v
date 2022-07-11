struct App {}

fn (mut app App) method_one() int {
	return 1
}

fn (mut app App) method_two() int {
	return 2
}

fn reflect_call(method_name string) int {
	a := App{}
	$for method in App.methods {
		if method.name == method_name {
			return a.$method()
		}
	}
	panic('Method not supported: $method_name')
}

fn test_comptime_call_type() {
	result := reflect_call('method_one')
	assert result == 1
}
