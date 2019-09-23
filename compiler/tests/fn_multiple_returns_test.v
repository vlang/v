struct UserData {
	group_id int
}

pub fn test_fn_multiple_returns() {
	name, age, data := fn_mr_get_user()
	assert name == 'joe'
	assert age == 34
	assert data.group_id == 1
	println('name: $name | age: $age | group_id: $data.group_id')
}

fn fn_mr_get_user() (string, int, UserData) {
	data := UserData{group_id: 1}
	return 'joe',34,data
}
