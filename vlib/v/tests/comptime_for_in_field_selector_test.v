fn print_field_values<T>(s T) {
	mut value_list := []string{}
	mut value_type_list := []string{}
	mut var_value_list := []string{}
	mut var_intp_value_list := []string{}

	$for field in T.fields {
		println(s.$(field.name))
		value_list << s.$(field.name).str()

		println(typeof(s.$(field.name)).name)
		value_type_list << typeof(s.$(field.name)).name

		val := s.$(field.name)
		println(val)
		var_value_list << val.str()
		var_intp_value_list << 'field value: ${val}'
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

	assert var_value_list.len == 4
	assert var_value_list[0] == 'Simon'
	assert var_value_list[1] == 'simon1234'
	assert var_value_list[2] == 'simon@gmail.com'
	assert var_value_list[3] == '15'

	assert var_intp_value_list.len == 4
	assert var_intp_value_list[0] == 'field value: Simon'
	assert var_intp_value_list[1] == 'field value: simon1234'
	assert var_intp_value_list[2] == 'field value: simon@gmail.com'
	assert var_intp_value_list[3] == 'field value: 15'
}

struct Foo {
	name     string
	password string [this_will_not_change_the_order]
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
