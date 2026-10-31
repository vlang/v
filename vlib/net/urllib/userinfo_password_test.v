import net.urllib

fn test_parse_preserves_explicit_empty_password() {
	urls := {
		'http://user:@example.com/':        'user'
		'http://:@example.com/':            ''
		'http://user%3Aname:@example.com/': 'user:name'
		'http://%3A:@example.com/':         ':'
	}
	for input, username in urls {
		url := urllib.parse(input)!
		userinfo := url.user or { panic('expected userinfo') }
		assert userinfo.username == username
		assert userinfo.password == ''
		assert userinfo.password_set
		assert url.str() == input
		roundtrip := urllib.parse(url.str())!
		roundtrip_user := roundtrip.user or { panic('expected userinfo after round trip') }
		assert roundtrip_user.username == username
		assert roundtrip_user.password == ''
		assert roundtrip_user.password_set
	}
}

fn test_parse_encoded_colon_username_without_password() {
	urls := {
		'http://user@example.com/':        'user'
		'http://user%3Aname@example.com/': 'user:name'
		'http://%3A@example.com/':         ':'
	}
	for input, username in urls {
		url := urllib.parse(input)!
		userinfo := url.user or { panic('expected userinfo') }
		assert userinfo.username == username
		assert userinfo.password == ''
		assert !userinfo.password_set
		assert url.str() == input
	}
}

fn test_parse_password_uses_first_literal_colon() {
	urls := {
		'http://:password@example.com/':        ['', 'password', 'http://:password@example.com/']
		'http://user:pass%3Aword@example.com/': ['user', 'pass:word',
			'http://user:pass%3Aword@example.com/']
		'http://user:a:b@example.com/':         ['user', 'a:b', 'http://user:a%3Ab@example.com/']
		'http://user::@example.com/':           ['user', ':', 'http://user:%3A@example.com/']
	}
	for input, expected in urls {
		url := urllib.parse(input)!
		userinfo := url.user or { panic('expected userinfo') }
		assert userinfo.username == expected[0]
		assert userinfo.password == expected[1]
		assert userinfo.password_set
		assert url.str() == expected[2]
	}
}

fn test_resolve_reference_preserves_explicit_empty_credentials() {
	base := urllib.parse('http://:@example.com/base')!
	resolved := base.resolve_reference(urllib.parse('next')!)!
	assert resolved.str() == 'http://:@example.com/next'
	userinfo := resolved.user or { panic('expected userinfo') }
	assert userinfo.username == ''
	assert userinfo.password == ''
	assert userinfo.password_set
}
