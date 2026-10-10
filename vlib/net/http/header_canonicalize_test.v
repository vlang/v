module http

// `render(canonicalize: true)` spells a field name by one of two rules:
// - a name that has a `CommonHeader` is spelled like `CommonHeader.str()`, which
//   for the names in the IANA HTTP Field Name Registry is the registered spelling;
// - any other name gets the first letter and each letter after a `-` in upper
//   case and the rest in lower case, like Go's `textproto.CanonicalMIMEHeaderKey`.
// See https://github.com/vlang/v/issues/29969.

// common_headers_not_mechanical lists every `CommonHeader` whose spelling is not
// the one of the second rule. Go, which has that rule only, spells them
// `Accept-Ch`, `Dnt`, `Etag`, `Sec-Websocket-Key`, `Www-Authenticate`, ...
const common_headers_not_mechanical = ['Accept-CH', 'Accept-CH-Lifetime', 'DNT', 'ETag', 'Expect-CT',
	'NEL', 'Sec-WebSocket-Accept', 'Sec-WebSocket-Key', 'SourceMap', 'TE', 'WWW-Authenticate',
	'X-DNS-Prefetch-Control', 'X-XSS-Protection']

// canonical_name returns the field name that `render(canonicalize: true)` writes
// for a field that was added as `name`.
fn canonical_name(name string) string {
	mut h := new_header()
	h.add_custom(name, 'v') or { panic(err) }
	return h.render(canonicalize: true).all_before(':')
}

// common_headers returns every `CommonHeader`.
fn common_headers() []CommonHeader {
	mut res := []CommonHeader{}
	for i := 0; true; i++ {
		header := CommonHeader.from(i) or { break }
		res << header
	}
	return res
}

// mechanical_name spells `name` by the rule of Go's `textproto.CanonicalMIMEHeaderKey`.
fn mechanical_name(name string) string {
	mut res := name.bytes()
	mut upper := true
	for i, c in res {
		if upper && c >= `a` && c <= `z` {
			res[i] = c - 32
		} else if !upper && c >= `A` && c <= `Z` {
			res[i] = c + 32
		}
		upper = c == `-`
	}
	return res.bytestr()
}

fn test_canonicalize_keeps_the_common_header_spelling_of_the_issue_names() {
	for name in ['www-authenticate', 'Www-Authenticate', 'WWW-Authenticate', 'WWW-AUTHENTICATE'] {
		assert canonical_name(name) == 'WWW-Authenticate'
	}
	for name in ['etag', 'Etag', 'ETag', 'ETAG'] {
		assert canonical_name(name) == 'ETag'
	}
	for name in ['te', 'Te', 'tE', 'TE'] {
		assert canonical_name(name) == 'TE'
	}
	for name in ['expect-ct', 'Expect-Ct', 'Expect-CT', 'EXPECT-CT'] {
		assert canonical_name(name) == 'Expect-CT'
	}
	for name in ['dnt', 'Dnt', 'DNT'] {
		assert canonical_name(name) == 'DNT'
	}
}

fn test_canonicalize_agrees_with_common_header_str() {
	headers := common_headers()
	assert headers.len > 100
	for header in headers {
		name := header.str()
		assert canonical_name(name) == name
		assert canonical_name(name.to_lower()) == name
		assert canonical_name(name.to_upper()) == name
		mut h := new_header()
		h.add(header, 'v')
		assert h.render(canonicalize: true) == '${name}: v\r\n'
	}
}

fn test_canonicalize_common_headers_that_differ_from_the_mechanical_rule() {
	names := common_headers().map(it.str())
	assert names.filter(mechanical_name(it) != it) == common_headers_not_mechanical
	for name in common_headers_not_mechanical {
		assert canonical_name(name.to_lower()) == name
		assert canonical_name(mechanical_name(name)) == name
	}
}

fn test_canonicalize_other_names_follow_the_mechanical_rule() {
	assert canonical_name('x-custom-header') == 'X-Custom-Header'
	assert canonical_name('x-CUSTOM-header') == 'X-Custom-Header'
	assert canonical_name('X-REQUEST-ID') == 'X-Request-Id'
	// a registered name that has no `CommonHeader` is not special
	assert canonical_name('Content-MD5') == 'Content-Md5'
	assert canonical_name('HTTP2-Settings') == 'Http2-Settings'
	assert canonical_name('P3P') == 'P3p'
	assert canonical_name('Sec-WebSocket-Version') == 'Sec-Websocket-Version'
	// single letters
	assert canonical_name('a') == 'A'
	assert canonical_name('Z') == 'Z'
	assert canonical_name('a-b') == 'A-B'
	// digits
	assert canonical_name('x-1st-header') == 'X-1st-Header'
	assert canonical_name('1abc') == '1abc'
	assert canonical_name('x9Y') == 'X9y'
	// leading, trailing and double hyphens
	assert canonical_name('-foo') == '-Foo'
	assert canonical_name('foo-') == 'Foo-'
	assert canonical_name('foo--bar') == 'Foo--Bar'
	assert canonical_name('-') == '-'
	// only a `-` starts a new word, and it is not the same as a `_`
	assert canonical_name('x_custom_header') == 'X_custom_header'
	assert canonical_name('x.y') == 'X.y'
	assert canonical_name('sec_websocket_key') == 'Sec_websocket_key'
}

fn test_canonicalize_other_names_match_the_mechanical_rule_exhaustively() {
	// every name of 1 to 5 characters over: a letter in each case, a digit,
	// the hyphen and another character that is valid in a field name
	alphabet := ['a', 'Z', '1', '-', '_']
	mut names := ['']
	mut checked := 0
	for _ in 0 .. 5 {
		mut longer := []string{cap: names.len * alphabet.len}
		for prefix in names {
			for c in alphabet {
				longer << prefix + c
			}
		}
		for name in longer {
			assert canonical_name(name) == mechanical_name(name), name
			checked++
		}
		names = longer.clone()
	}
	assert checked == 5 + 25 + 125 + 625 + 3125
}

fn test_canonicalize_changes_the_rendered_name_only() {
	mut h := new_header()
	h.add_custom('etag', 'a')!
	h.add_custom('x-CUSTOM-header', 'b')!
	assert h.render() == 'etag: a\r\nx-CUSTOM-header: b\r\n'
	assert h.render(canonicalize: true) == 'ETag: a\r\nX-Custom-Header: b\r\n'
	// HTTP/2 field names are lower case, with or without `canonicalize`
	assert h.render(version: .v2_0, canonicalize: true) == 'etag: a\r\nx-custom-header: b\r\n'
	// the header keeps the spelling it was given
	assert h.keys() == ['etag', 'x-CUSTOM-header']
}
