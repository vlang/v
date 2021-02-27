import net.http

fn test_header_new() {
	http.new_header() or { panic('should not error') }
}

fn test_header_invalid_key() {
	http.new_header(
		{key: 'space is invalid', value: ':('},
	) or { return }
	panic('should have returned')
}

fn test_header_canonicalizes_key() {
	h := http.new_header(
		{key: 'any-case', value: 'abc'},
		{key: 'any-case', value: 'def'},
	) or { panic('should not error') }

	value := h.get('ANY-CASE') or { panic('should get value') }
	assert value == 'abc'
}

fn test_header_adds_multiple() {
	mut h := http.new_header() or { panic('should not error') }
	h.add(http.CommonHeader.accept, 'one') or { panic('should add') }
	h.add(http.CommonHeader.accept, 'two') or { panic('should add') }

	assert h.values(http.CommonHeader.accept) == ['one' 'two']
}

fn test_header_set() {
	mut h := http.new_header(
		{key: http.CommonHeader.to, value: 'one'},
		{key: http.CommonHeader.to, value: 'two'}
	) or { panic('should not error') }
	assert h.values(http.CommonHeader.to) == ['one' 'two']
	h.set('to', 'three')
	assert h.values(http.CommonHeader.to) == ['three']
}

fn test_header_delete() {
	mut h := http.new_header(
		{key: http.CommonHeader.to, value: 'one'},
		{key: http.CommonHeader.to, value: 'two'}
	) or { panic('should not error') }
	assert h.values(http.CommonHeader.to) == ['one' 'two']
	h.delete('to')
	assert h.values(http.CommonHeader.to) == []
}
