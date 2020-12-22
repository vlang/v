import cli

fn test_if_string_flag_parses() {
	mut flag := cli.Flag{
		flag: .string
		name: 'flag'
	}
	flag.parse(['-flag', 'value'], false) or { panic(err) }
	assert flag.value == 'value'
	flag.parse(['-flag=value'], false) or { panic(err) }
	assert flag.value == 'value'
}

fn test_if_bool_flag_parses() {
	mut flag := cli.Flag{
		flag: .bool
		name: 'flag'
	}
	mut value := false
	flag.parse(['-flag'], false) or { panic(err) }
	value = flag.get_bool() or { panic(err) }
	assert value == true
	flag.parse(['-flag', 'true'], false) or { panic(err) }
	value = flag.get_bool() or { panic(err) }
	assert value == true
	flag.parse(['-flag=true'], false) or { panic(err) }
	value = flag.get_bool() or { panic(err) }
	assert value == true
	flag.parse(['-flag', 'false'], false) or { panic(err) }
	value = flag.get_bool() or { panic(err) }
	assert value == false
	flag.parse(['-flag=false'], false) or { panic(err) }
	value = flag.get_bool() or { panic(err) }
	assert value == false
}

fn test_if_int_flag_parses() {
	mut flag := cli.Flag{
		flag: .int
		name: 'flag'
	}
	mut value := 0
	flag.parse(['-flag', '42'], false) or { panic(err) }
	value = flag.get_int() or { panic(err) }
	assert value == 42
	flag.parse(['-flag=42'], false) or { panic(err) }
	value = flag.get_int() or { panic(err) }
	assert value == 42
}

fn test_if_float_flag_parses() {
	mut flag := cli.Flag{
		flag: .float
		name: 'flag'
	}
	mut value := f64(0)
	flag.parse(['-flag', '3.14159'], false) or { panic(err) }
	value = flag.get_float() or { panic(err) }
	assert value == 3.14159
	flag.parse(['-flag=3.14159'], false) or { panic(err) }
	assert flag.value.f64() == 3.14159
	value = flag.get_float() or { panic(err) }
	assert value == 3.14159
}

fn test_if_flag_parses_with_abbrev() {
	mut flag := cli.Flag{
		flag: .bool
		name: 'flag'
		abbrev: 'f'
	}
	mut value := false
	flag.parse(['--flag'], true) or { panic(err) }
	value = flag.get_bool() or { panic(err) }
	assert value == true
	flag.parse(['-flag'], true) or { panic(err) }
	value = flag.get_bool() or { panic(err) }
	assert value == true
}
