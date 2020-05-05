fn test_array_to_string_conversion() {
	a := ['1', '2', '3', '4']
	assert a.str() == "['1', '2', '3', '4']"

	b := [1, 2, 3, 4]
	assert b.str() == '[1, 2, 3, 4]'

	c := [1.1, 2.2, 3.3, 4.4]
	assert c.str() == '[1.1, 2.2, 3.3, 4.4]'

	d := [i16(1), 2, 3]
	assert d.str() == '[1, 2, 3]'

	e := [i64(1), 2, 3]
	assert e.str() == '[1, 2, 3]'
}

fn test_interpolation_array_to_string() {
	a := ['1', '2', '3']
	assert '$a' == "['1', '2', '3']"

	b := [1, 2, 3, 4]
	assert '$b' == '[1, 2, 3, 4]'

	c := [1.1, 2.2, 3.3, 4.4]
	assert '$c' == '[1.1, 2.2, 3.3, 4.4]'

	d := [i16(1), 2, 3]
	assert '$d' == '[1, 2, 3]'

	e := [i64(1), 2, 3]
	assert '$e' == '[1, 2, 3]'
}

fn test_interpolation_array_of_map_to_string() {
	mut ams := []map[string]string{}
	ams << {'a': 'b', 'c': 'd'}
	ams << {'e': 'f', 'g': 'h'}
	assert '$ams' == "[{'a': 'b', 'c': 'd'}, {'e': 'f', 'g': 'h'}]"
}
