struct UserData {
	test string
}

fn test_fn_multiple_returns() {
	name, age, groups, data := fn_mr_get_user()
	assert name == 'joe'
	assert age == 34
	assert groups[0] == 'admins' 
	assert groups[1] == 'users'
	assert data.test == 'Test Data'
	println('name: $name | age: $age | groups: ' + groups.join(',') + ' | data: $data.test')
}

fn fn_mr_get_user() (string, int, []string, UserData) {
	groups := ['admins', 'users']
	data := UserData{test: 'Test Data'}
	return 'joe',34,groups,data
}
