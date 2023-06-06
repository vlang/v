fn test_main() {
	mut arr := [2][3][2]map[string]string{}
	arr[0][0][1]['key'] = 'value'
	dump(arr)
	assert arr[0][0][1] == {
		'key': 'value'
	}
}
