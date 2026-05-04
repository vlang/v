fn test_nested_array_of_string_references_compare_empty_in_mut_loop() {
	v1 := 'abc'
	v2 := 'def'
	mut sec_array := [][]&string{len: 1, init: []&string{}}
	sec_array[0] << &v1
	sec_array[0] << &v2
	mut found_non_empty := false
	for mut elem in sec_array {
		if elem != [] {
			found_non_empty = true
			assert elem.len == 2
			assert *elem[0] == 'abc'
			assert *elem[1] == 'def'
		}
	}
	assert found_non_empty
}
