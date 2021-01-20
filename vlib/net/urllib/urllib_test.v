// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import net.urllib

fn test_net_urllib() {
	test_query := 'Hellö Wörld@vlang'
	assert urllib.query_escape(test_query) == 'Hell%C3%B6+W%C3%B6rld%40vlang'

	test_url := 'https://joe:pass@www.mydomain.com:8080/som/url?param1=test1&param2=test2&foo=bar#testfragment'
	u := urllib.parse(test_url) or {
		assert false
		return
	}
	assert u.scheme     == 'https' &&
		u.hostname()    == 'www.mydomain.com' &&
		u.port()        == '8080' &&
		u.path          == '/som/url' &&
		u.fragment      == 'testfragment' &&
		u.user.username == 'joe' &&
		u.user.password == 'pass'
}

fn test_str() {
	url := urllib.parse("https://en.wikipedia.org/wiki/Brazil_(1985_film)") or {
		panic("unable to parse URL")
	}
	assert url.str() == 'https://en.wikipedia.org/wiki/Brazil_(1985_film)'
}

fn test_escape_unescape() {
	original := 'те ст: т\\%'
	escaped := urllib.query_escape(original)
	assert escaped == '%D1%82%D0%B5+%D1%81%D1%82%3A+%D1%82%5C%25'
	unescaped := urllib.query_unescape(escaped) or { assert false return }
	assert unescaped == original
}
