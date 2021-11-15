fn print_field_values<T>(s T) {
	mut value_list := []string{}
	mut value_type_list := []string{}

	$for field in T.fields {
		println(s.$(field.name))
		value_list << s.$(field.name).str()

		println(typeof(s.$(field.name)).name)
		value_type_list << typeof(s.$(field.name)).name
	}
	assert value_list.len == 4
	assert value_list[0] == 'Simon'
	assert value_list[1] == 'simon1234'
	assert value_list[2] == 'simon@gmail.com'
	assert value_list[3] == '15'

	assert value_type_list.len == 4
	assert value_type_list[0] == 'string'
	assert value_type_list[1] == 'string'
	assert value_type_list[2] == 'string'
	assert value_type_list[3] == 'int'
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
