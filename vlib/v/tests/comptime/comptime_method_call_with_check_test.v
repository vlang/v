struct TestStruct {}

fn (t TestStruct) one_arg(a string) string {
	println(a)
	return a
}

fn (t TestStruct) two_args(a string, b int) string {
	println('${a}:${b}')
	return a
}

fn test_main() {
	t := TestStruct{}
	mut out := ''
	$for method in TestStruct.methods {
		$if method.name == 'one_arg' {
			res := t.$method(method.name)
			out += res
		} $else $if method.name == 'two_args' {
			res := t.$method(method.name, 42)
			out += res
		}
	}
	assert out == 'one_argtwo_args'
}
