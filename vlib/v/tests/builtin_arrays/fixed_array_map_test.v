fn test_main() {
	mut arr := [22]map[string]string{}
	arr[0]['key'] = 'value'
	arr[1]['key2'] = 'value2'
	dump(arr[0])
	assert dump(arr[0]) == {
		'key': 'value'
	}
	assert dump(arr[1]) == {
		'key2': 'value2'
	}
	assert dump(arr).len == 22
}
