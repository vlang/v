module main

struct MyStruct {
	id1 string @[sql: 'id\"']
	id2 string @[sql: 'id']
	id3 string @[sql: id]
	id4 string @[sql: 'id"']
}

fn test_main() {
	mut arr := []string{}
	$for f in MyStruct.fields {
		arr << f.attrs[0]
	}
	assert arr[0] == 'sql: "id""'
	assert arr[1] == 'sql: "id"'
	assert arr[2] == 'sql: id'
	assert arr[3] == 'sql: "id""'
}
