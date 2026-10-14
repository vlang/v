import net.http

// Names that are not valid HTTP field names (an RFC 9110 `token`). The first
// five are the rows of issue #29970: empty, space, `=`, control byte and
// non-ASCII. The lowercase ones would visibly change if they were
// canonicalised instead of being returned as they are.
const invalid_names = [
	'',
	'X Custom',
	'X=C',
	'X-\x01Y',
	'X-\xc3\xa9',
	'x custom',
	'x=c',
	'x-\x01y',
	'x-\xc3\xa9',
	' accept',
	'accept ',
	'accept:',
	'accept\tencoding',
	'x-a\r\nx-b',
	'x-a\nx-b',
	'x-a: 1\r\nx-b',
	'x-\x00',
	'x-\x7f',
	'x-\xff',
	'x(y)',
	'x/y',
	'x@y',
	'"x"',
]

// is_tchar is the `tchar` rule of RFC 9110, written out independently of the module.
fn is_tchar(b u8) bool {
	return b.is_alnum() || r"!#$%&'*+-.^_`|~".contains_u8(b)
}

fn test_canonical_header_key_returns_an_invalid_name_unchanged() {
	for name in invalid_names {
		assert http.canonical_header_key(name) == name, 'name bytes: ${name.bytes()}'
	}
}

fn test_canonical_header_key_accepts_a_valid_name_in_any_case() {
	expected := {
		'accept-encoding':     'Accept-Encoding'
		'ACCEPT-ENCODING':     'Accept-Encoding'
		'Accept-Encoding':     'Accept-Encoding'
		'aCCEPT-eNCODING':     'Accept-Encoding'
		'content-type':        'Content-Type'
		'CONTENT-TYPE':        'Content-Type'
		'host':                'Host'
		'HOST':                'Host'
		'uSER-aGENT':          'User-Agent'
		'if-modified-SINCE':   'If-Modified-Since'
		'x-custom-header':     'X-Custom-Header'
		'X-CUSTOM-HEADER':     'X-Custom-Header'
		'x-Custom-HEADER':     'X-Custom-Header'
		'x-request-id':        'X-Request-Id'
		'a':                   'A'
		'x_custom':            'X_custom'
		'X_CUSTOM':            'X_custom'
		'x.y!z':               'X.y!z'
		'a--b':                'A--B'
		'-a':                  '-A'
		'x-1abc':              'X-1abc'
		'1abc-def':            '1abc-Def'
		r"x-!#$%&'*+.^_`|~-y": r"X-!#$%&'*+.^_`|~-Y"
	}
	for name, want in expected {
		assert http.canonical_header_key(name) == want, name
		// The canonical form is a fixed point.
		assert http.canonical_header_key(want) == want, want
	}
}

// The spelling is the one the module already writes for `canonicalize: true`,
// also for the well known names that are not plain Title-Case.
fn test_canonical_header_key_matches_render_canonicalize() {
	names := ['accept', 'Content-Type', 'etag', 'ETAG', 'dnt', 'te', 'www-authenticate',
		'x-xss-protection', 'sec-websocket-key', 'sec-websocket-accept', 'x-custom', 'X_CUSTOM']
	for name in names {
		mut h := http.new_header()
		h.add_custom(name, 'v')!
		want := '${http.canonical_header_key(name)}: v\r\n'
		assert h.render(version: .v1_1, canonicalize: true) == want, name
	}
}

// Every byte is either canonicalised, or makes the name invalid, and that
// is decided exactly as `add_custom` decides to accept or to reject the name.
fn test_canonical_header_key_treats_each_byte_as_add_custom_does() {
	for i in 0 .. 256 {
		b := u8(i)
		name := [u8(`x`), b, `y`].bytestr()
		got := http.canonical_header_key(name)
		mut h := http.new_header()
		if is_tchar(b) {
			want := if b == `-` { 'X-Y' } else { 'X' + [b].bytestr().to_lower() + 'y' }
			assert got == want, 'byte ${i}'
			h.add_custom(name, 'v') or { assert false, 'add_custom rejected byte ${i}' }
		} else {
			assert got == name, 'byte ${i}'
			h.add_custom(name, 'v') or { continue }
			assert false, 'add_custom accepted byte ${i}'
		}
	}
}

// An invalid name still does not get into a Header: nothing checks the name
// again when the header is rendered or sent.
fn test_add_custom_and_set_custom_still_reject_an_invalid_name() {
	for name in invalid_names {
		mut h := http.new_header()
		mut rejected := 0
		h.add_custom(name, 'v') or {
			assert err.msg().starts_with('Invalid header key')
			rejected++
		}
		h.set_custom(name, 'v') or {
			assert err.msg().starts_with('Invalid header key')
			rejected++
		}
		assert rejected == 2, 'name bytes: ${name.bytes()}'
		assert h.keys().len == 0
		assert h.render() == ''
		mut kvs := map[string]string{}
		kvs[name] = 'v'
		if _ := http.new_custom_header_from_map(kvs) {
			assert false, 'new_custom_header_from_map accepted name bytes: ${name.bytes()}'
		}
	}
}
