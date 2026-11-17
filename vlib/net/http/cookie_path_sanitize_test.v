import net.http

// The expected results are the ones of Go's net/http `sanitizeCookiePath` /
// `validCookiePathByte`, which this module ported: bytes with 0x20 <= b < 0x7f
// are kept, `;` is dropped because it is the Set-Cookie attribute separator,
// and `!` is a valid path byte (0x21).
struct CookieStrCase {
	name    string // the cookie name
	path    string // the path given to the cookie
	written string // what `str()` serializes for the whole cookie
}

const cookie_str_cases = [
	CookieStrCase{ name: 'c', path: '', written: 'c=v' },
	CookieStrCase{ name: 'c', path: '/docs', written: 'c=v; Path=/docs' },
	CookieStrCase{ name: 'c', path: '/dir/sub', written: 'c=v; Path=/dir/sub' },
	CookieStrCase{ name: 'c', path: '/', written: 'c=v; Path=/' },
	// `;` is the attribute separator: it must not be emitted
	CookieStrCase{ name: 'c', path: '/a;b=c', written: 'c=v; Path=/ab=c' },
	CookieStrCase{
		name:    'c'
		path:    '/x; Domain=.example.net'
		written: 'c=v; Path=/x Domain=.example.net'
	},
	// `;` alone is dropped entirely, and the attribute is still emitted
	CookieStrCase{ name: 'c', path: ';', written: 'c=v; Path=' },
	// `!` is a valid path byte (0x21): it is preserved
	CookieStrCase{ name: 'c', path: '/a!b', written: 'c=v; Path=/a!b' },
	CookieStrCase{ name: 'c', path: '/!a', written: 'c=v; Path=/!a' },
	// other bytes the predicate rejects are dropped, as in Go
	CookieStrCase{ name: 'c', path: '\x00a\tb\r\nc', written: 'c=v; Path=abc' },
	CookieStrCase{ name: 'c', path: 'é', written: 'c=v; Path=' },
	CookieStrCase{ name: 'c', path: 'a\x7fb', written: 'c=v; Path=ab' },
	// a path that makes an invalid cookie name still emits nothing
	CookieStrCase{ name: 'a b', path: '/docs', written: '' },
]

fn test_str_serializes_the_sanitized_cookie_path() {
	for case in cookie_str_cases {
		cookie := http.Cookie{
			name:  case.name
			value: 'v'
			path:  case.path
		}
		assert cookie.str() == case.written
	}
}

// regression: `;` must never reach a serialized Path attribute, where any
// Set-Cookie reader would parse it as an injected attribute
fn test_str_never_injects_attributes_through_the_path() {
	cookie := http.Cookie{
		name:  'session'
		value: 'v'
		path:  '/x; HttpOnly; Domain=.example.net'
	}
	raw := cookie.str()
	assert raw == 'session=v; Path=/x HttpOnly Domain=.example.net'
	// the only `;` on the wire is the separator after the value
	assert raw.count(';') == 1
}
