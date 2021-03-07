struct TestStruct {}

fn (t TestStruct) test(arg1 string, arg2 string, arg3 string) {}

fn test_comptime_method_names() {
	$for method in TestStruct.methods {
		if method.name == 'test' {
			args := method.args
			assert args[0].name == 'arg1'
			assert args[1].name == 'arg2'
			assert args[2].name == 'arg3'
		}
	}
}
