import net.http

struct SameSiteWriteCase {
	same_site http.SameSite
	attribute string
}

struct SameSiteReadCase {
	line      string        // a Set-Cookie header value, as received
	same_site http.SameSite // the mode the parser assigns to it
	rewritten string        // what the parsed cookie serialises back to
}

// Only Lax, Strict and None have a serialisation. `SameSite` is not a valid attribute
// without a value, and the default mode is what a client assumes when it is absent.
const same_site_write_cases = [
	SameSiteWriteCase{
		same_site: .same_site_not_set
		attribute: ''
	},
	SameSiteWriteCase{
		same_site: .same_site_default_mode
		attribute: ''
	},
	SameSiteWriteCase{
		same_site: .same_site_lax_mode
		attribute: '; SameSite=Lax'
	},
	SameSiteWriteCase{
		same_site: .same_site_strict_mode
		attribute: '; SameSite=Strict'
	},
	SameSiteWriteCase{
		same_site: .same_site_none_mode
		attribute: '; SameSite=None'
	},
]

const same_site_read_cases = [
	SameSiteReadCase{
		line:      'c=v'
		same_site: .same_site_not_set
		rewritten: 'c=v'
	},
	SameSiteReadCase{
		line:      'c=v; SameSite'
		same_site: .same_site_default_mode
		rewritten: 'c=v'
	},
	SameSiteReadCase{
		line:      'c=v; SameSite='
		same_site: .same_site_default_mode
		rewritten: 'c=v'
	},
	SameSiteReadCase{
		line:      'c=v; SameSite=xyz'
		same_site: .same_site_default_mode
		rewritten: 'c=v'
	},
	SameSiteReadCase{
		line:      'c=v; Secure; SameSite; HttpOnly'
		same_site: .same_site_default_mode
		rewritten: 'c=v; HttpOnly; Secure'
	},
	SameSiteReadCase{
		line:      'c=v; SameSite=Lax'
		same_site: .same_site_lax_mode
		rewritten: 'c=v; SameSite=Lax'
	},
	SameSiteReadCase{
		line:      'c=v; samesite=LAX'
		same_site: .same_site_lax_mode
		rewritten: 'c=v; SameSite=Lax'
	},
	SameSiteReadCase{
		line:      'c=v; SameSite=Strict'
		same_site: .same_site_strict_mode
		rewritten: 'c=v; SameSite=Strict'
	},
	SameSiteReadCase{
		line:      'c=v; SameSite=None; Secure'
		same_site: .same_site_none_mode
		rewritten: 'c=v; Secure; SameSite=None'
	},
]

// parse_set_cookie parses one Set-Cookie header value the way a client reads a response.
fn parse_set_cookie(line string) http.Cookie {
	mut header := http.new_header()
	header.add(.set_cookie, line)
	cookies := http.Response{
		header: header
	}.cookies()
	assert cookies.len == 1, line
	return cookies[0]
}

fn test_cookie_str_writes_same_site_only_with_a_value() {
	for tc in same_site_write_cases {
		bare := http.Cookie{
			name:      'c'
			value:     'v'
			same_site: tc.same_site
		}
		assert bare.str() == 'c=v${tc.attribute}', tc.same_site.str()
		// The attribute stays last, and the other attributes are written either way.
		full := http.Cookie{
			name:      'c'
			value:     'v'
			http_only: true
			secure:    true
			same_site: tc.same_site
		}
		assert full.str() == 'c=v; HttpOnly; Secure${tc.attribute}', tc.same_site.str()
	}
}

fn test_cookie_str_never_writes_a_bare_same_site_attribute() {
	for tc in same_site_write_cases {
		cookie := http.Cookie{
			name:      'c'
			value:     'v'
			same_site: tc.same_site
		}
		for attribute in cookie.str().split('; ')[1..] {
			assert attribute.starts_with('SameSite='), '${tc.same_site}: `${attribute}`'
			assert attribute.len > 'SameSite='.len, '${tc.same_site}: `${attribute}`'
		}
	}
}

fn test_set_cookie_same_site_is_parsed_and_serialised_back() {
	for tc in same_site_read_cases {
		cookie := parse_set_cookie(tc.line)
		assert cookie.same_site == tc.same_site, tc.line
		assert cookie.str() == tc.rewritten, tc.line
		// The serialised form is stable: a second trip writes the same header value.
		again := parse_set_cookie(tc.rewritten)
		assert again.str() == tc.rewritten, tc.line
		// A mode that has a value survives the trip. The default mode has none, so it
		// comes back as an absent attribute, which a client treats the same way.
		if tc.same_site == .same_site_default_mode {
			assert again.same_site == .same_site_not_set, tc.line
		} else {
			assert again.same_site == tc.same_site, tc.line
		}
	}
}
