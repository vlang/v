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

	b := dump(a['key2'])

	assert b? == MyStruct{
		field: 10
	}

	assert a['key1'] == none
	assert a['key2'] != none
}
