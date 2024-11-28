import net.http

struct SetCookieTestCase {
	cookie &http.Cookie
	raw    string
}

struct ReadSetCookiesTestCase {
	header  map[string][]string
	cookies []&http.Cookie
}

struct AddCookieTestCase {
	cookie []&http.Cookie
	raw    string
}

const write_set_cookie_tests = [
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'cookie-1'
			value: 'v1'
		}
		raw:    'cookie-1=v1'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:    'cookie-2'
			value:   'two'
			max_age: 3600
		}
		raw:    'cookie-2=two; Max-Age=3600'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:   'cookie-3'
			value:  'three'
			domain: '.example.com'
		}
		raw:    'cookie-3=three; domain=example.com'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'cookie-4'
			value: 'four'
			path:  '/restricted/'
		}
		raw:    'cookie-4=four; path=/restricted/'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:   'cookie-5'
			value:  'five'
			domain: 'wrong;bad.abc'
		}
		raw:    'cookie-5=five'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:   'cookie-6'
			value:  'six'
			domain: 'bad-.abc'
		}
		raw:    'cookie-6=six'
	},
	// SetCookieTestCase{
	// 	cookie: &http.Cookie{name: 'cookie-7', value: 'seven', domain: '127.0.0.1'},
	// 	raw: 'cookie-7=seven; domain=127.0.0.1'
	// },
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:   'cookie-8'
			value:  'eight'
			domain: '::1'
		}
		raw:    'cookie-8=eight'
	},
	// {
	// 	cookie: &http.Cookie{name: 'cookie-9', value: 'expiring', expires: time.unix(1257894000, 0)},
	// 	'cookie-9=expiring; Expires=Tue, 10 Nov 2009 23:00:00 GMT',
	// },
	// According to IETF 6265 Section 5.1.1.5, the year cannot be less than 1601
	// SetCookieTestCase{
	// 	cookie: &http.Cookie{name: 'cookie-10', value: 'expiring-1601', expires: time.parse('Mon, 01 Jan 1601 01:01:01 GMT')},
	// 	raw: 'cookie-10=expiring-1601; Expires=Mon, 01 Jan 1601 01:01:01 GMT'
	// },
	// SetCookieTestCase{
	// 	cookie: &http.Cookie{name: 'cookie-11', value: 'invalid-expiry', expires: time.parse('Mon, 01 Jan 1600 01:01:01 GMT')},
	// 	raw: 'cookie-11=invalid-expiry'
	// },
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:      'cookie-12'
			value:     'samesite-default'
			same_site: .same_site_default_mode
		}
		raw:    'cookie-12=samesite-default; SameSite'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:      'cookie-13'
			value:     'samesite-lax'
			same_site: .same_site_lax_mode
		}
		raw:    'cookie-13=samesite-lax; SameSite=Lax'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:      'cookie-14'
			value:     'samesite-strict'
			same_site: .same_site_strict_mode
		}
		raw:    'cookie-14=samesite-strict; SameSite=Strict'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:      'cookie-15'
			value:     'samesite-none'
			same_site: .same_site_none_mode
		}
		raw:    'cookie-15=samesite-none; SameSite=None'
	},
	// The 'special' cookies have values containing commas or spaces which
	// are disallowed by RFC 6265 but are common in the wild.
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'special-1'
			value: 'a z'
		}
		raw:    'special-1=a z'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'special-2'
			value: ' z'
		}
		raw:    'special-2=" z"'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'special-3'
			value: 'a '
		}
		raw:    'special-3="a "'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'special-4'
			value: ' '
		}
		raw:    'special-4=" "'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'special-5'
			value: 'a,z'
		}
		raw:    'special-5=a,z'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'special-6'
			value: ',z'
		}
		raw:    'special-6=",z"'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'special-7'
			value: 'a,'
		}
		raw:    'special-7="a,"'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'special-8'
			value: ','
		}
		raw:    'special-8=","'
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'empty-value'
			value: ''
		}
		raw:    'empty-value='
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name: ''
		}
		raw:    ''
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name: '\t'
		}
		raw:    ''
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name: '\r'
		}
		raw:    ''
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'a\nb'
			value: 'v'
		}
		raw:    ''
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'a\nb'
			value: 'v'
		}
		raw:    ''
	},
	SetCookieTestCase{
		cookie: &http.Cookie{
			name:  'a\rb'
			value: 'v'
		}
		raw:    ''
	},
]
const add_cookies_tests = [
	AddCookieTestCase{
		cookie: []
		raw:    ''
	},
	AddCookieTestCase{
		cookie: [&http.Cookie{
			name:  'cookie-1'
			value: 'v1'
		}]
		raw:    'cookie-1=v1'
	},
	AddCookieTestCase{
		cookie: [&http.Cookie{
			name:  'cookie-1'
			value: 'v1'
		}, &http.Cookie{
			name:  'cookie-2'
			value: 'v2'
		},
			&http.Cookie{
				name:  'cookie-3'
				value: 'v3'
			}]
		raw:    'cookie-1=v1; cookie-2=v2; cookie-3=v3'
	},
]

fn test_write_set_cookies() {
	for _, tt in write_set_cookie_tests {
		assert tt.cookie.str() == tt.raw
	}
}
