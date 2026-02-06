import json

struct User {
	name string
}

struct MyStruct {
	user   &User //
	users  map[string]User
	users2 map[string]&User
}

fn test_json_encode_with_ptr() {
	user := User{
		name: 'foo'
	}
	data := MyStruct{
		user:   &user
		users:  {
			'keyfoo': user
		}
		users2: {
			'keyfoo': &user
		}
	}

	assert json.encode(data) == '{"user":{"name":"foo"},"users":{"keyfoo":{"name":"foo"}},"users2":{"keyfoo":{"name":"foo"}}}'
}
