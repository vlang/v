import net.http

// RFC 9110 section 5.6.2: token = 1*tchar. A cookie-name is a token (RFC 6265 section 4.1.1).
const token_punctuation = "!#$%&'*+-.^_`|~"
const token_alphanumerics = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
// The delimiters that RFC 9110 excludes from a token.
const separators = '"(),/:;<=>?@[\\]{}'

fn cookie_str(name string) string {
	return http.Cookie{
		name:  name
		value: 'v'
	}.str()
}

fn test_str_drops_the_reported_names() {
	// https://github.com/vlang/v/issues/29965
	for name in ['a b', 'a;b', 'a,b', 'a=b', 'a\tb'] {
		assert cookie_str(name) == '', 'name: ${name.bytes().hex()}'
	}
}

fn test_str_drops_a_name_with_any_separator() {
	assert separators.len == 17
	for sep in separators.bytes() {
		s := sep.ascii_str()
		for name in [s, s + 'a', 'a' + s, 'a' + s + 'b'] {
			assert cookie_str(name) == '', 'name: ${name}'
		}
	}
}

fn test_str_writes_every_token_character() {
	assert token_punctuation.len == 15
	for p in token_punctuation.bytes() {
		s := p.ascii_str()
		for name in [s, s + 'a', 'a' + s, 'a' + s + 'b'] {
			assert cookie_str(name) == '${name}=v'
		}
	}
	assert cookie_str(token_punctuation) == '${token_punctuation}=v'
	assert cookie_str(token_alphanumerics) == '${token_alphanumerics}=v'
}

fn test_str_writes_a_name_only_when_every_byte_is_a_token_character() {
	token_chars := token_punctuation + token_alphanumerics
	assert token_chars.len == 77
	mut written := 0
	for i in 0 .. 256 {
		b := u8(i)
		name := [u8(`a`), b, `z`].bytestr()
		alone := [b].bytestr()
		if token_chars.contains_u8(b) {
			assert cookie_str(name) == '${name}=v'
			assert cookie_str(alone) == '${alone}=v'
			written++
		} else {
			assert cookie_str(name) == '', 'byte: ${i}'
			assert cookie_str(alone) == '', 'byte: ${i}'
		}
	}
	assert written == 77
}

fn test_str_drops_control_and_non_ascii_names() {
	for name in ['', ' ', 'a\nb', 'a\rb', 'a\r\nInjected: 1', 'a\x00b', 'a\x1fb', 'a\x7fb', 'a\x80b',
		'a\xffb', 'ключ', 'naïve', '日本'] {
		assert cookie_str(name) == '', 'name: ${name.bytes().hex()}'
	}
}

fn test_str_writes_nothing_at_all_for_an_invalid_name() {
	attributes := http.Cookie{
		name:      'a;b'
		value:     'v'
		path:      '/'
		domain:    'example.com'
		max_age:   60
		secure:    true
		http_only: true
		same_site: .same_site_strict_mode
	}
	assert attributes.str() == ''
	valid := http.Cookie{
		...attributes
		name: 'a.b'
	}
	assert valid.str().starts_with('a.b=v; ')
}

// Reading stays more lenient than writing: a name of visible ASCII bytes that a peer
// sent is returned, although `str()` would not write it.
const readable_names = ['foo[bar]', 'a:b', 'a/b', 'a@b', 'a,b', '(a)', '{a}', 'a?b', '<a>', 'a"b',
	'a\\b', 'tok.en-1_~']
const unreadable_names = ['', 'a b', 'a\tb', 'a\x00b', 'a\x7fb', 'a\x80b', 'ключ']

fn test_read_cookies_keeps_visible_ascii_names() {
	for name in readable_names {
		cookies := http.read_cookies(http.new_header(key: .cookie, value: '${name}=v'), '')
		assert cookies.len == 1, 'name: ${name}'
		assert cookies[0].name == name
		assert cookies[0].value == 'v'
	}
	for name in unreadable_names {
		cookies := http.read_cookies(http.new_header(key: .cookie, value: '${name}=v'), '')
		assert cookies.len == 0, 'name: ${name.bytes().hex()}'
	}
	// `;` and `=` end a name, so neither reaches the name check; `a b` is dropped.
	header := http.new_header(key: .cookie, value: 'foo[bar]=1; a b=2; a:b=3; c=d=e; ok=4')
	cookies := http.read_cookies(header, '')
	assert cookies.map('${it.name} ${it.value}') == ['foo[bar] 1', 'a:b 3', 'c d=e', 'ok 4']
	filtered := http.read_cookies(header, 'a:b')
	assert filtered.len == 1
	assert filtered[0].value == '3'
}

fn test_set_cookie_parsing_keeps_visible_ascii_names() {
	for name in readable_names {
		response := http.Response{
			header: http.new_header(key: .set_cookie, value: '${name}=v; Path=/')
		}
		cookies := response.cookies()
		assert cookies.len == 1, 'name: ${name}'
		assert cookies[0].name == name
		assert cookies[0].value == 'v'
		assert cookies[0].path == '/'
	}
	for name in unreadable_names {
		response := http.Response{
			header: http.new_header(key: .set_cookie, value: '${name}=v; Path=/')
		}
		assert response.cookies().len == 0, 'name: ${name.bytes().hex()}'
	}
}

fn request_cookie_value(req http.Request, name string) string {
	if cookie := req.cookie(name) {
		return cookie.value
	}
	return '<none>'
}

fn test_request_cookies_keep_visible_ascii_names() {
	head := 'GET / HTTP/1.1\r\nHost: example.com\r\nCookie: foo[bar]=1; a:b=2; a b=3\r\n\r\n'
	req := http.parse_request_head_str(head)!
	assert request_cookie_value(req, 'foo[bar]') == '1'
	assert request_cookie_value(req, 'a:b') == '2'
	assert request_cookie_value(req, 'a b') == '<none>'
}

fn test_a_cookie_read_with_a_non_token_name_is_not_written_back() {
	response := http.Response{
		header: http.new_header(key: .set_cookie, value: 'foo[bar]=v; Path=/')
	}
	cookies := response.cookies()
	assert cookies.len == 1
	assert cookies[0].str() == ''
}
