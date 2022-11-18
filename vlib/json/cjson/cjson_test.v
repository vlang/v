import json.cjson

fn test_object_with_null() {
	mut root := cjson.create_object()
	root.add_item_to_object('name', cjson.create_string('Andre'))
	root.add_item_to_object('age', cjson.create_null())
	assert root.print_unformatted() == '{"name":"Andre","age":null}'
}

fn test_creating_complex_json() {
	mut root := cjson.create_array()
	root.add_item_to_array(cjson.create_string('user'))
	mut obj := cjson.create_object()
	obj.add_item_to_object('username', cjson.create_string('foo'))
	obj.add_item_to_object('password', cjson.create_string('bar'))
	root.add_item_to_array(obj)
	result := root.print_unformatted()
	println(result)

	assert result == '["user",{"username":"foo","password":"bar"}]'
}
