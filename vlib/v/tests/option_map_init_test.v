struct MyStruct {
	field int
}

fn empty() map[string]?MyStruct {
	return {
		'key1': ?MyStruct(none)
		'key2': ?MyStruct{
			field: 10
		}
	}
}

fn test_main() {
	a := dump(empty())

	assert a['key1'] == none
	assert a['key2'] == none
}
