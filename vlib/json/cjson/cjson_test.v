import json.cjson

fn test_object_with_null() {
	mut root := cjson.create_object()
	root.add_item_to_object('name', cjson.create_string('Andre'))
	root.add_item_to_object('age', cjson.create_null())
	assert root.print_unformatted() == '{"name":"Andre","age":null}'
}
