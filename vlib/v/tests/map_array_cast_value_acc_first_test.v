const m = {
	'foo': [u8(0xf0), 0x00]
	'bar': [0xba, 0xaa]
	'foobar': [12, 45]
}

fn test_check_arr_elem_type() {
	assert typeof(m['foo']).name == '[]u8'
	assert typeof(m['bar']).name == '[]u8'
	assert typeof(m['foobar']).name == '[]u8'
}

fn test_get_elem() {
	assert '${m['foo']}' == '[240, 0]'
	assert '${m['bar']}' == '[186, 170]'
	assert '${m['foobar']}' == '[12, 45]'
}

