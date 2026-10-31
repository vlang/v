// vtest vflags: -w
import json2

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

	assert json2.encode(data, escape_unicode: true) == '{"user":{"name":"foo"},"users":{"keyfoo":{"name":"foo"}},"users2":{"keyfoo":{"name":"foo"}}}'
}
