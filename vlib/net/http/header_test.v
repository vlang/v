module http

fn test_header_new() {
	h := http.new_header(
		{key: .accept, value: 'nothing'},
		{key: .expires, value: 'yesterday'}
	)
	assert h.contains(.accept)
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

fn test_header_get() ? {
	mut h := http.new_header(key: .dnt, value: 'one')
	h.add_custom('dnt', 'two') ?
	dnt := h.get_custom('dnt') or { '' }
	exact := h.get_custom('dnt', exact: true) or { '' }
	assert dnt == 'one'
	assert exact == 'two'
}

fn test_header_set() ? {
	mut h := http.new_header(
		{key: .dnt, value: 'one'},
		{key: .dnt, value: 'two'}
	)
	assert h.values(.dnt) == ['one' 'two']
	h.set_custom('DNT', 'three') ?
	assert h.values(.dnt) == ['three']
}

fn test_header_delete() {
	mut h := http.new_header(
		{key: .dnt, value: 'one'},
		{key: .dnt, value: 'two'}
	)
	assert h.values(.dnt) == ['one' 'two']
	h.delete(.dnt)
	assert h.values(.dnt) == []
}

fn test_header_delete_not_existing() {
	mut h := http.new_header()
	assert h.data.len == 0
	assert h.keys.len == 0
	h.delete(.dnt)
	assert h.data.len == 0
	assert h.keys.len == 0
}

fn test_custom_header() ? {
	mut h := http.new_header()
	h.add_custom('AbC', 'dEf') ?
	h.add_custom('aBc', 'GhI') ?
	assert h.custom_values('AbC', exact: true) == ['dEf']
	assert h.custom_values('aBc', exact: true) == ['GhI']
	assert h.custom_values('ABC') == ['dEf', 'GhI']
	assert h.custom_values('abc') == ['dEf', 'GhI']
	assert h.keys() == ['AbC', 'aBc']
	h.delete_custom('AbC')
	h.delete_custom('aBc')

	h.add_custom('abc', 'def') ?
	assert h.custom_values('abc') == ['def']
	assert h.custom_values('ABC') == ['def']
	assert h.keys() == ['abc']
	h.delete_custom('abc')

	h.add_custom('accEPT', '*/*') ?
	assert h.custom_values('ACCept') == ['*/*']
	assert h.values(.accept) == ['*/*']
	assert h.keys() == ['accEPT']
}

fn test_contains_custom() ? {
	mut h := http.new_header()
	h.add_custom('Hello', 'world') ?
	assert h.contains_custom('hello')
	assert h.contains_custom('HELLO')
	assert h.contains_custom('Hello', exact: true)
	assert h.contains_custom('hello', exact: true) == false
	assert h.contains_custom('HELLO', exact: true) == false
}

fn test_get_custom() ? {
	mut h := http.new_header()
	h.add_custom('Hello', 'world') ?
	assert h.get_custom('hello') ? == 'world'
	assert h.get_custom('HELLO') ? == 'world'
	assert h.get_custom('Hello', exact: true) ? == 'world'
	if _ := h.get_custom('hello', exact: true) {
		// should be none
		assert false
	}
	if _ := h.get_custom('HELLO', exact: true) {
		// should be none
		assert false
	}
}

fn test_custom_values() ? {
	mut h := http.new_header()
	h.add_custom('Hello', 'world') ?
	assert h.custom_values('hello') == ['world']
	assert h.custom_values('HELLO') == ['world']
	assert h.custom_values('Hello', exact: true) == ['world']
	assert h.custom_values('hello', exact: true) == []
	assert h.custom_values('HELLO', exact: true) == []
}

fn test_coerce() ? {
	mut h := http.new_header()
	h.add_custom('accept', 'foo') ?
	h.add(.accept, 'bar')
	assert h.values(.accept) == ['foo', 'bar']
	assert h.keys().len == 2

	h.coerce()
	assert h.values(.accept) == ['foo', 'bar']
	assert h.keys() == ['accept'] // takes the first occurrence
}

fn test_coerce_canonicalize() ? {
	mut h := http.new_header()
	h.add_custom('accept', 'foo') ?
	h.add(.accept, 'bar')
	assert h.values(.accept) == ['foo', 'bar']
	assert h.keys().len == 2

	h.coerce(canonicalize: true)
	assert h.values(.accept) == ['foo', 'bar']
	assert h.keys() == ['Accept'] // canonicalize header
}

fn test_coerce_custom() ? {
	mut h := http.new_header()
	h.add_custom('Hello', 'foo') ?
	h.add_custom('hello', 'bar') ?
	h.add_custom('HELLO', 'baz') ?
	assert h.custom_values('hello') == ['foo', 'bar', 'baz']
	assert h.keys().len == 3

	h.coerce()
	assert h.custom_values('hello') == ['foo', 'bar', 'baz']
	assert h.keys() == ['Hello'] // takes the first occurrence
}

fn test_coerce_canonicalize_custom() ? {
	mut h := http.new_header()
	h.add_custom('foo-BAR', 'foo') ?
	h.add_custom('FOO-bar', 'bar') ?
	assert h.custom_values('foo-bar') == ['foo', 'bar']
	assert h.keys().len == 2

	h.coerce(canonicalize: true)
	assert h.custom_values('foo-bar') == ['foo', 'bar']
	assert h.keys() == ['Foo-Bar'] // capitalizes the header
}

fn test_render_version() ? {
	mut h := http.new_header()
	h.add_custom('accept', 'foo') ?
	h.add_custom('Accept', 'bar') ?
	h.add(.accept, 'baz')

	s1_0 := h.render(version: .v1_0)
	assert s1_0.contains('accept: foo\n\r')
	assert s1_0.contains('Accept: bar,baz\n\r')

	s1_1 := h.render(version: .v1_1)
	assert s1_1.contains('accept: foo\n\r')
	assert s1_1.contains('Accept: bar,baz\n\r')

	s2_0 := h.render(version: .v2_0)
	assert s2_0.contains('accept: foo\n\r')
	assert s2_0.contains('accept: bar,baz\n\r')
}

fn test_render_coerce() ? {
	mut h := http.new_header()
	h.add_custom('accept', 'foo') ?
	h.add_custom('Accept', 'bar') ?
	h.add(.accept, 'baz')
	h.add(.host, 'host')

	s1_0 := h.render(version: .v1_1, coerce: true)
	assert s1_0.contains('accept: foo,bar,baz\n\r')
	assert s1_0.contains('Host: host\n\r')

	s1_1 := h.render(version: .v1_1, coerce: true)
	assert s1_1.contains('accept: foo,bar,baz\n\r')
	assert s1_1.contains('Host: host\n\r')

	s2_0 := h.render(version: .v2_0, coerce: true)
	assert s2_0.contains('accept: foo,bar,baz\n\r')
	assert s2_0.contains('host: host\n\r')
}

fn test_render_canonicalize() ? {
	mut h := http.new_header()
	h.add_custom('accept', 'foo') ?
	h.add_custom('Accept', 'bar') ?
	h.add(.accept, 'baz')
	h.add(.host, 'host')

	s1_0 := h.render(version: .v1_1, canonicalize: true)
	assert s1_0.contains('Accept: foo\n\r')
	assert s1_0.contains('Accept: bar,baz\n\r')
	assert s1_0.contains('Host: host\n\r')

	s1_1 := h.render(version: .v1_1, canonicalize: true)
	assert s1_1.contains('Accept: foo\n\r')
	assert s1_1.contains('Accept: bar,baz\n\r')
	assert s1_1.contains('Host: host\n\r')

	s2_0 := h.render(version: .v2_0, canonicalize: true)
	assert s2_0.contains('accept: foo\n\r')
	assert s2_0.contains('accept: bar,baz\n\r')
	assert s2_0.contains('host: host\n\r')
}

fn test_render_coerce_canonicalize() ? {
	mut h := http.new_header()
	h.add_custom('accept', 'foo') ?
	h.add_custom('Accept', 'bar') ?
	h.add(.accept, 'baz')
	h.add(.host, 'host')

	s1_0 := h.render(version: .v1_1, coerce: true, canonicalize: true)
	assert s1_0.contains('Accept: foo,bar,baz\n\r')
	assert s1_0.contains('Host: host\n\r')

	s1_1 := h.render(version: .v1_1, coerce: true, canonicalize: true)
	assert s1_1.contains('Accept: foo,bar,baz\n\r')
	assert s1_1.contains('Host: host\n\r')

	s2_0 := h.render(version: .v2_0, coerce: true, canonicalize: true)
	assert s2_0.contains('accept: foo,bar,baz\n\r')
	assert s2_0.contains('host: host\n\r')
}

fn test_str() ? {
	mut h := http.new_header()
	h.add(.accept, 'text/html')
	h.add_custom('Accept', 'image/jpeg') ?
	h.add_custom('X-custom', 'Hello') ?

	// key order is not guaranteed
	assert h.str() == 'Accept: text/html,image/jpeg\n\rX-custom: Hello\n\r'
		|| h.str() == 'X-custom: Hello\n\rAccept:text/html,image/jpeg\n\r'
}
