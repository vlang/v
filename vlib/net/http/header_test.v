import net.http

fn test_header_new() {
	h := http.new_header(
		{key: .accept, value: 'nothing'},
		{key: .expires, value: 'yesterday'}
	)
	assert h.contains_custom('accept')
	assert h.contains(.expires)
	accept := h.get(.accept) or { '' }
	expires := h.get(.expires) or { '' }
	assert accept == 'nothing'
	assert expires == 'yesterday'
}

fn test_header_invalid_key() {
	mut h := http.new_header()
	h.add_custom('space is invalid', ':(') or { return }
	panic('should have returned')
}

fn test_header_adds_multiple() {
	mut h := http.new_header()
	h.add(.accept, 'one')
	h.add(.accept, 'two')

	assert h.values(.accept) == ['one' 'two']
}

fn test_header_set() {
	mut h := http.new_header(
		{key: .dnt, value: 'one'},
		{key: .dnt, value: 'two'}
	)
	assert h.values(.dnt) == ['one' 'two']
	h.set_custom('dnt', 'three')
	assert h.values(.dnt) == ['three']
}

fn test_header_delete() {
	mut h := http.new_header(
		{key: .dnt, value: 'one'},
		{key: .dnt, value: 'two'}
	)
	assert h.values(.dnt) == ['one' 'two']
	h.delete_custom('dnt')
	assert h.values(.dnt) == []
}

fn test_custom_header() ? {
	mut h := http.new_header()
	h.add_custom('AbC', 'dEf') ?
	h.add_custom('aBc', 'GhI') ?
	assert h.custom_values('abc') == ['dEf', 'GhI']
	assert h.custom_values('ABC') == ['dEf', 'GhI']
	assert h.keys() == ['AbC']
	h.delete_custom('abc')

	h.add_custom('abc', 'def') ?
	assert h.custom_values('abc') == ['def']
	assert h.custom_values('ABC') == ['def']
	assert h.keys() == ['abc']
	h.delete_custom('abc')

	h.add_custom('accEPT', '*/*') ?
	assert h.custom_values('ACCept') == ['*/*']
	assert h.values(.accept) == ['*/*']
	// should be coerced because it is a common header
	assert h.keys() == ['Accept']
}

fn test_str() ? {
	mut h := http.new_header()
	h.add(.accept, 'text/html')
	h.add_custom('accept', 'image/jpeg') ?
	h.add_custom('X-custom', 'Hello') ?
	h.add_custom('x-custom', 'world') ?

	// order is not guaranteed
	assert h.str() == 'Accept: text/html,image/jpeg\n\rX-custom: Hello,world\n\r'
		|| h.str() == 'X-custom: Hello,world\n\rAccept:text/html,image/jpeg\n\r'
}
