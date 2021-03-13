struct TestStruct {
mut:
	one_arg_called    bool
	two_args_called   bool
	three_args_called bool
}

fn (mut t TestStruct) one_arg(a1 string) {
	t.one_arg_called = true
}
fn (mut t TestStruct) two_args(a2 string, b2 int) {
	t.two_args_called = true
}
fn (mut t TestStruct) three_args(a3 string, b3 int, c3 []string) {
	t.three_args_called = true
}

fn test_comptime_method_names() {
	mut num_methods := 0
	$for method in TestStruct.methods {
		if method.name == 'one_arg' {
			assert method.args[0].name == 'a1'
			num_methods++
		} else if method.name == 'two_args' {
			assert method.args[0].name == 'a2'
			assert method.args[1].name == 'b2'
			num_methods++
		} else if method.name == 'three_args' {
			assert method.args[0].name == 'a3'
			assert method.args[1].name == 'b3'
			assert method.args[2].name == 'c3'
			num_methods++
		}
	}
	assert num_methods == 3
}

fn test_comptime_call_method() {
	mut t := TestStruct{}
	$for method in TestStruct.methods {
		if method.name == 'one_arg' {
			t.$method('one')
		} else if method.name == 'two_args' {
			t.$method('two', 2)
		} else if method.name == 'three_args' {
			t.$method('three', 3, ['th' 'ree'])
		}
	}
	assert t.one_arg_called
	assert t.two_args_called
	assert t.three_args_called
}
