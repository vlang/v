module http

fn test_header_new() {
	h := new_header(HeaderConfig{ key: .accept, value: 'nothing' },
		key: .expires
		value: 'yesterday'
	)
	assert h.contains(.accept)
	assert h.contains(.expires)
	accept := h.get(.accept) or { '' }
	expires := h.get(.expires) or { '' }
	assert accept == 'nothing'
	assert expires == 'yesterday'
}

fn test_header_invalid_key() {
	mut h := new_header()
	h.add_custom('space is invalid', ':(') or { return }
	panic('should have returned')
}

fn test_header_adds_multiple() {
	mut h := new_header()
	h.add(.accept, 'one')
	h.add(.accept, 'two')

	assert h.values(.accept) == ['one', 'two']
}

fn test_header_get() {
	mut h := new_header(key: .dnt, value: 'one')
	h.add_custom('dnt', 'two')?
	dnt := h.get_custom('dnt') or { '' }
	exact := h.get_custom('dnt', exact: true) or { '' }
	assert dnt == 'one'
	assert exact == 'two'
}

fn test_header_set() {
	mut h := new_header(HeaderConfig{ key: .dnt, value: 'one' },
		key: .dnt
		value: 'two'
	)
	assert h.values(.dnt) == ['one', 'two']
	h.set_custom('DNT', 'three')?
	assert h.values(.dnt) == ['three']
}

fn test_header_delete() {
	mut h := new_header(HeaderConfig{ key: .dnt, value: 'one' },
		key: .dnt
		value: 'two'
	)
	assert h.values(.dnt) == ['one', 'two']
	h.delete(.dnt)
	assert h.values(.dnt) == []
}

fn test_header_delete_not_existing() {
	mut h := new_header()
	assert h.data.len == 0
	assert h.keys.len == 0
	h.delete(.dnt)
	assert h.data.len == 0
	assert h.keys.len == 0
}

fn test_custom_header() {
	mut h := new_header()
	h.add_custom('AbC', 'dEf')?
	h.add_custom('aBc', 'GhI')?
	assert h.custom_values('AbC', exact: true) == ['dEf']
	assert h.custom_values('aBc', exact: true) == ['GhI']
	assert h.custom_values('ABC') == ['dEf', 'GhI']
	assert h.custom_values('abc') == ['dEf', 'GhI']
	assert h.keys() == ['AbC', 'aBc']
	h.delete_custom('AbC')
	h.delete_custom('aBc')

	h.add_custom('abc', 'def')?
	assert h.custom_values('abc') == ['def']
	assert h.custom_values('ABC') == ['def']
	assert h.keys() == ['abc']
	h.delete_custom('abc')

	h.add_custom('accEPT', '*/*')?
	assert h.custom_values('ACCept') == ['*/*']
	assert h.values(.accept) == ['*/*']
	assert h.keys() == ['accEPT']
}

fn test_contains_custom() {
	mut h := new_header()
	h.add_custom('Hello', 'world')?
	assert h.contains_custom('hello')
	assert h.contains_custom('HELLO')
	assert h.contains_custom('Hello', exact: true)
	assert h.contains_custom('hello', exact: true) == false
	assert h.contains_custom('HELLO', exact: true) == false
}

fn test_get_custom() {
	mut h := new_header()
	h.add_custom('Hello', 'world')?
	assert h.get_custom('hello')? == 'world'
	assert h.get_custom('HELLO')? == 'world'
	assert h.get_custom('Hello', exact: true)? == 'world'
	if _ := h.get_custom('hello', exact: true) {
		// should be none
		assert false
	}
	if _ := h.get_custom('HELLO', exact: true) {
		// should be none
		assert false
	}
}

fn test_starting_with() {
	mut h := new_header()
	h.add_custom('Hello-1', 'world')?
	h.add_custom('Hello-21', 'world')?
	assert h.starting_with('Hello-')? == 'Hello-1'
	assert h.starting_with('Hello-2')? == 'Hello-21'
}

fn test_custom_values() {
	mut h := new_header()
	h.add_custom('Hello', 'world')?
	assert h.custom_values('hello') == ['world']
	assert h.custom_values('HELLO') == ['world']
	assert h.custom_values('Hello', exact: true) == ['world']
	assert h.custom_values('hello', exact: true) == []
	assert h.custom_values('HELLO', exact: true) == []
}

fn test_coerce() {
	mut h := new_header()
	h.add_custom('accept', 'foo')?
	h.add(.accept, 'bar')
	assert h.values(.accept) == ['foo', 'bar']
	assert h.keys().len == 2

	h.coerce()
	assert h.values(.accept) == ['foo', 'bar']
	assert h.keys() == ['accept'] // takes the first occurrence
}

fn test_coerce_canonicalize() {
	mut h := new_header()
	h.add_custom('accept', 'foo')?
	h.add(.accept, 'bar')
	assert h.values(.accept) == ['foo', 'bar']
	assert h.keys().len == 2

	h.coerce(canonicalize: true)
	assert h.values(.accept) == ['foo', 'bar']
	assert h.keys() == ['Accept'] // canonicalize header
}

fn test_coerce_custom() {
	mut h := new_header()
	h.add_custom('Hello', 'foo')?
	h.add_custom('hello', 'bar')?
	h.add_custom('HELLO', 'baz')?
	assert h.custom_values('hello') == ['foo', 'bar', 'baz']
	assert h.keys().len == 3

	h.coerce()
	assert h.custom_values('hello') == ['foo', 'bar', 'baz']
	assert h.keys() == ['Hello'] // takes the first occurrence
}

fn test_coerce_canonicalize_custom() {
	mut h := new_header()
	h.add_custom('foo-BAR', 'foo')?
	h.add_custom('FOO-bar', 'bar')?
	assert h.custom_values('foo-bar') == ['foo', 'bar']
	assert h.keys().len == 2

	h.coerce(canonicalize: true)
	assert h.custom_values('foo-bar') == ['foo', 'bar']
	assert h.keys() == ['Foo-Bar'] // capitalizes the header
}

fn test_render_version() {
	mut h := new_header()
	h.add_custom('accept', 'foo')?
	h.add_custom('Accept', 'bar')?
	h.add(.accept, 'baz')

	s1_0 := h.render(version: .v1_0)
	assert s1_0.contains('accept: foo\r\n')
	assert s1_0.contains('Accept: bar\r\n')
	assert s1_0.contains('Accept: baz\r\n')

	s1_1 := h.render(version: .v1_1)
	assert s1_1.contains('accept: foo\r\n')
	assert s1_1.contains('Accept: bar\r\n')
	assert s1_1.contains('Accept: baz\r\n')

	s2_0 := h.render(version: .v2_0)
	assert s2_0.contains('accept: foo\r\n')
	assert s2_0.contains('accept: bar\r\n')
	assert s2_0.contains('accept: baz\r\n')
}

fn test_render_coerce() {
	mut h := new_header()
	h.add_custom('accept', 'foo')?
	h.add_custom('Accept', 'bar')?
	h.add(.accept, 'baz')
	h.add(.host, 'host')

	s1_0 := h.render(version: .v1_1, coerce: true)
	assert s1_0.contains('accept: foo\r\n')
	assert s1_0.contains('accept: bar\r\n')
	assert s1_0.contains('accept: baz\r\n')
	assert s1_0.contains('Host: host\r\n')

	s1_1 := h.render(version: .v1_1, coerce: true)
	assert s1_1.contains('accept: foo\r\n')
	assert s1_1.contains('accept: bar\r\n')
	assert s1_1.contains('accept: baz\r\n')
	assert s1_1.contains('Host: host\r\n')

	s2_0 := h.render(version: .v2_0, coerce: true)
	assert s2_0.contains('accept: foo\r\n')
	assert s2_0.contains('accept: bar\r\n')
	assert s2_0.contains('accept: baz\r\n')
	assert s2_0.contains('host: host\r\n')
}

fn test_render_canonicalize() {
	mut h := new_header()
	h.add_custom('accept', 'foo')?
	h.add_custom('Accept', 'bar')?
	h.add(.accept, 'baz')
	h.add(.host, 'host')

	s1_0 := h.render(version: .v1_1, canonicalize: true)
	assert s1_0.contains('Accept: foo\r\n')
	assert s1_0.contains('Accept: bar\r\n')
	assert s1_0.contains('Accept: baz\r\n')
	assert s1_0.contains('Host: host\r\n')

	s1_1 := h.render(version: .v1_1, canonicalize: true)
	assert s1_1.contains('Accept: foo\r\n')
	assert s1_1.contains('Accept: bar\r\n')
	assert s1_1.contains('Accept: baz\r\n')
	assert s1_1.contains('Host: host\r\n')

	s2_0 := h.render(version: .v2_0, canonicalize: true)
	assert s2_0.contains('accept: foo\r\n')
	assert s2_0.contains('accept: bar\r\n')
	assert s2_0.contains('accept: baz\r\n')
	assert s2_0.contains('host: host\r\n')
}

fn test_render_coerce_canonicalize() {
	mut h := new_header()
	h.add_custom('accept', 'foo')?
	h.add_custom('Accept', 'bar')?
	h.add(.accept, 'baz')
	h.add(.host, 'host')

	s1_0 := h.render(version: .v1_1, coerce: true, canonicalize: true)
	assert s1_0.contains('Accept: foo\r\n')
	assert s1_0.contains('Accept: bar\r\n')
	assert s1_0.contains('Accept: baz\r\n')
	assert s1_0.contains('Host: host\r\n')

	s1_1 := h.render(version: .v1_1, coerce: true, canonicalize: true)
	assert s1_1.contains('Accept: foo\r\n')
	assert s1_1.contains('Accept: bar\r\n')
	assert s1_1.contains('Accept: baz\r\n')
	assert s1_1.contains('Host: host\r\n')

	s2_0 := h.render(version: .v2_0, coerce: true, canonicalize: true)
	assert s2_0.contains('accept: foo\r\n')
	assert s2_0.contains('accept: bar\r\n')
	assert s2_0.contains('accept: baz\r\n')
	assert s2_0.contains('host: host\r\n')
}

fn test_str() {
	mut h := new_header()
	h.add(.accept, 'text/html')
	h.add_custom('Accept', 'image/jpeg')?
	h.add_custom('X-custom', 'Hello')?

	// key order is not guaranteed
	assert h.str() == 'Accept: text/html\r\nAccept: image/jpeg\r\nX-custom: Hello\r\n'
		|| h.str() == 'X-custom: Hello\r\nAccept:text/html\r\nAccept: image/jpeg\r\n'
}

fn test_header_from_map() {
	h := new_header_from_map({
		CommonHeader.accept:  'nothing'
		CommonHeader.expires: 'yesterday'
	})
	assert h.contains(.accept)
	assert h.contains(.expires)
	assert h.get(.accept) or { '' } == 'nothing'
	assert h.get(.expires) or { '' } == 'yesterday'
}

fn test_custom_header_from_map() {
	h := new_custom_header_from_map({
		'Server': 'VWeb'
		'foo':    'bar'
	})?
	assert h.contains_custom('server')
	assert h.contains_custom('foo')
	assert h.get_custom('server') or { '' } == 'VWeb'
	assert h.get_custom('foo') or { '' } == 'bar'
}

fn test_header_join() {
	h1 := new_header_from_map({
		CommonHeader.accept:  'nothing'
		CommonHeader.expires: 'yesterday'
	})
	h2 := new_custom_header_from_map({
		'Server': 'VWeb'
		'foo':    'bar'
	})?
	h3 := h1.join(h2)
	// h1 is unchanged
	assert h1.contains(.accept)
	assert h1.contains(.expires)
	assert !h1.contains_custom('Server')
	assert !h1.contains_custom('foo')
	// h2 is unchanged
	assert !h2.contains(.accept)
	assert !h2.contains(.expires)
	assert h2.contains_custom('Server')
	assert h2.contains_custom('foo')
	// h3 has all four headers
	assert h3.contains(.accept)
	assert h3.contains(.expires)
	assert h3.contains_custom('Server')
	assert h3.contains_custom('foo')
}

fn parse_headers_test(s string, expected map[string]string) ? {
	assert parse_headers(s)? == new_custom_header_from_map(expected)?
}

fn test_parse_headers() ? {
	parse_headers_test('foo: bar', {
		'foo': 'bar'
	})?
	parse_headers_test('foo: \t  bar', {
		'foo': 'bar'
	})?
	parse_headers_test('foo: bar\r\n\tbaz', {
		'foo': 'bar baz'
	})?
	parse_headers_test('foo: bar \r\n\tbaz\r\n   buzz', {
		'foo': 'bar baz buzz'
	})?
	parse_headers_test('foo: bar\r\nbar:baz', {
		'foo': 'bar'
		'bar': 'baz'
	})?
	parse_headers_test('foo: bar\r\nbar:baz\r\n', {
		'foo': 'bar'
		'bar': 'baz'
	})?
	parse_headers_test('foo: bar\r\nbar:baz\r\n\r\n', {
		'foo': 'bar'
		'bar': 'baz'
	})?
	assert parse_headers('foo: bar\r\nfoo:baz')?.custom_values('foo') == ['bar', 'baz']

	if x := parse_headers(' oops: oh no') {
		return error('should have errored, but got ${x}')
	}
}

fn test_set_cookie() {
	// multiple Set-Cookie headers should be sent when rendered
	mut h := new_header()
	h.add(.set_cookie, 'foo')
	h.add(.set_cookie, 'bar')
	assert h.render() == 'Set-Cookie: foo\r\nSet-Cookie: bar\r\n'
}
