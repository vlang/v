fn print_field_values<T>(s T) {
	mut value_list := []string{}

	$for field in T.fields {
		println(s.$(field.name))
		value_list << s.$(field.name).str()
	}
	assert value_list.len == 4
	assert value_list[0] == 'Simon'
	assert value_list[1] == 'simon1234'
	assert value_list[2] == 'simon@gmail.com'
	assert value_list[3] == '15'
}

struct Foo {
	name     string
	password string
	email    string
	age      int
}

fn test_comptime_for_in_field_selector() {
	bar := Foo{
		name: 'Simon'
		password: 'simon1234'
		email: 'simon@gmail.com'
		age: 15
	}
	print_field_values<Foo>(bar)
}
