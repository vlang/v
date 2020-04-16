import cli

fn test_if_string_flag_parses() {
	mut flag := cli.Flag{
		flag: .string,
		name: 'flag',
	}

	flag.parse(['--flag', 'value']) or { panic(err) }
	assert flag.value == 'value'

	flag.parse(['--flag=value']) or { panic(err) }
	assert flag.value == 'value'
}

fn test_if_bool_flag_parses() {
	mut flag := cli.Flag{
		flag: .bool,
		name: 'flag',
	}

	flag.parse(['--flag']) or { panic(err) }
	assert flag.value == 'true'

	flag.parse(['--flag', 'true']) or { panic(err) }
	assert flag.value == 'true'

	flag.parse(['--flag=true']) or { panic(err) }
	assert flag.value == 'true'
}

fn test_if_int_flag_parses() {
	mut flag := cli.Flag{
		flag: .int,
		name: 'flag',
	}

	flag.parse(['--flag', '42']) or { panic(err) }
	assert flag.value.int() == 42

	flag.parse(['--flag=42']) or { panic(err) }
	assert flag.value.int() == 42
}

fn test_if_float_flag_parses() {
	mut flag := cli.Flag{
		flag: .float,
		name: 'flag',
	}

	flag.parse(['--flag', '3.14159']) or { panic(err) }
	assert flag.value.f64() == 3.14159

	flag.parse(['--flag=3.14159']) or { panic(err) }
	assert flag.value.f64() == 3.14159
}
