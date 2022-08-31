// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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
	assert u.scheme == 'https' && u.hostname() == 'www.mydomain.com' && u.port() == '8080'
		&& u.path == '/som/url' && u.fragment == 'testfragment' && u.user.username == 'joe'
		&& u.user.password == 'pass'
}

fn test_str() {
	url := urllib.parse('https://en.wikipedia.org/wiki/Brazil_(1985_film)') or {
		panic('unable to parse URL')
	}
	assert url.str() == 'https://en.wikipedia.org/wiki/Brazil_(1985_film)'
}

fn test_escape_unescape() {
	original := 'те ст: т\\%'
	escaped := urllib.query_escape(original)
	assert escaped == '%D1%82%D0%B5+%D1%81%D1%82%3A+%D1%82%5C%25'
	unescaped := urllib.query_unescape(escaped) or {
		assert false
		return
	}
	assert unescaped == original
}

fn test_parse_query() ? {
	q1 := urllib.parse_query('format=%22%25l%3A+%25c+%25t%22')?
	q2 := urllib.parse_query('format="%l:+%c+%t"')?
	// dump(q1)
	// dump(q2)
	assert q1.get('format') == '"%l: %c %t"'
	assert q2.get('format') == '"%l: %c %t"'
}

fn test_parse_query_orders() ? {
	query_one := urllib.parse_query('https://someapi.com/endpoint?gamma=zalibaba&tau=1&alpha=alibaba&signature=alibaba123')?
	qvalues := query_one.values()
	assert qvalues == ['zalibaba', '1', 'alibaba', 'alibaba123']
}

fn test_parse_missing_host() ? {
	// issue #10311
	url := urllib.parse('http:///')?
	assert url.str() == 'http://///'
}

// testing the case where the key as a null value
// e.g ?key=
fn test_parse_none_value() ? {
	query_one := urllib.parse_query('gamma=zalibaba&tau=1&alpha=alibaba&signature=')?
	qvalues := query_one.values()
	qvalues_map := query_one.to_map()
	assert qvalues == ['zalibaba', '1', 'alibaba']
	assert qvalues_map == {
		'gamma':     ['zalibaba']
		'tau':       ['1']
		'alpha':     ['alibaba']
		'signature': ['']
	}
}

// testing the case where the query as empity value
// e.g https://www.vlang.dev?alibaba
fn test_parse_empty_query_one() ? {
	query_str := 'alibaba'
	query_one := urllib.parse_query(query_str)?
	qvalues := query_one.values()
	qvalues_map := query_one.to_map()
	query_encode := query_one.encode()
	assert qvalues == []
	assert qvalues_map == {
		'alibaba': ['']
	}
	assert query_str == query_encode
}

// testing the case where the query as empity value
// e.g https://www.vlang.dev?
fn test_parse_empty_query_two() ? {
	query_str := ''
	query_one := urllib.parse_query(query_str)?
	qvalues := query_one.values()
	qvalues_map := query_one.to_map()
	query_encode := query_one.encode()
	assert qvalues == []
	assert qvalues_map == {}
	assert query_str == query_encode
}

fn test_parse() ? {
	urls := [
		'jdbc:mysql://test_user:ouupppssss@localhost:3306/sakila?profileSQL=true',
		'ftp://ftp.is.co.za/rfc/rfc1808.txt',
		'http://www.ietf.org/rfc/rfc2396.txt#header1',
		'ldap://[2001:db8::7]/c=GB?objectClass=one&objectClass=two',
		'mailto:John.Doe@example.com',
		'news:comp.infosystems.www.servers.unix',
		'tel:+1-816-555-1212',
		'telnet://192.0.2.16:80/',
		'urn:oasis:names:specification:docbook:dtd:xml:4.1.2',
		'foo://example.com:8042/over/there?name=ferret#nose',
		'ftp://2001:0db8:85a3:0000:0000:8a2e:0370:7334/path/file.txt',
		'ws://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]:4000',
	]
	for url in urls {
		_ := urllib.parse(url) or {
			eprintln(err)
			assert false
			return
		}
	}
}
