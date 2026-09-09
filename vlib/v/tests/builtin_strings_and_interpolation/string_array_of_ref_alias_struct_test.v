type Issue24154Order = Issue24154Asset

struct Issue24154Asset {
	id string
}

fn test_string_array_of_ref_alias_struct() {
	mut list_orders := []&Issue24154Order{}
	list_orders << &Issue24154Order{
		id: '2'
	}
	list_orders << &Issue24154Order{
		id: '3'
	}
	s := '${list_orders}'
	assert s.contains("id: '2'")
	assert s.contains("id: '3'")
}

fn test_string_fixed_array_of_ref_alias_struct() {
	list_orders := [&Issue24154Order{
		id: '4'
	}, &Issue24154Order{
		id: '5'
	}]!
	s := '${list_orders}'
	assert s.contains("id: '4'")
	assert s.contains("id: '5'")
}
