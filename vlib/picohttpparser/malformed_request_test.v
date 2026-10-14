module picohttpparser

// Malformed and edge-case input coverage for `parse_request`. Each row was
// measured on this tree; the error strings are the parser's own.

struct MalformedCase {
	input       string
	ret         int
	num_headers int
	err         string
}

const malformed_cases = [
	// valid baselines: any HTTP/1.x minor version digit is accepted
	MalformedCase{
		input: 'GET / HTTP/1.0\r\n\r\n'
		ret:   18
	},
	MalformedCase{
		input: 'GET / HTTP/1.1\r\n\r\n'
		ret:   18
	},
	MalformedCase{
		input: 'GET / HTTP/1.9\r\n\r\n'
		ret:   18
	},
	// version must be HTTP/1.x
	MalformedCase{
		input: 'GET / HTTP/2.0\r\n\r\n'
		err:   'error parsing request: picohttpparser only supports HTTP/1.x'
	},
	MalformedCase{
		input: 'GET / http/1.1\r\n\r\n'
		err:   'error parsing request: picohttpparser only supports HTTP/1.x'
	},
	MalformedCase{
		input: 'GET / HTTP/1.x\r\n\r\n'
		err:   'error parsing request: invalid HTTP version'
	},
	// bare LF line endings are accepted everywhere
	MalformedCase{
		input:       'GET / HTTP/1.1\nHost: x\n\n'
		ret:         24
		num_headers: 1
	},
	// truncated requests report -2 (incomplete), not an error
	MalformedCase{
		input: 'GET / HTTP/1.1'
		ret:   -2
	},
	MalformedCase{
		input: 'GET / HTTP/1.1\r\n'
		ret:   -2
	},
	MalformedCase{
		input: 'GET / HTTP/1.1\r\nHo'
		ret:   -2
	},
	MalformedCase{
		input:       'GET / HTTP/1.1\r\nHost: x\r\n'
		ret:         -2
		num_headers: 1
	},
	// request line shapes
	MalformedCase{
		input: 'GET\t/ HTTP/1.1\r\n\r\n'
		err:   'error parsing request: invalid character "9"'
	},
	MalformedCase{
		input: 'GET/ HTTP/1.1\r\n\r\n'
		err:   'error parsing request: invalid character "13"'
	},
	MalformedCase{
		input: '  GET / HTTP/1.1\r\n\r\n'
		err:   'error parsing request: invalid method or path'
	},
	// header shapes
	MalformedCase{
		input: 'GET / HTTP/1.1\r\n: v\r\n\r\n'
		err:   'error parsing request: invalid header name'
	},
	MalformedCase{
		input: 'GET / HTTP/1.1\r\nHost : v\r\n\r\n'
		err:   'error parsing request: invalid character in header "32"'
	},
	MalformedCase{
		input: 'GET / HTTP/1.1\r\nNoColon\r\n\r\n'
		// `N` is a tchar, so the name scan now runs to the CR.
		err:   'error parsing request: invalid character in header "13"'
	},
	// a control byte in a header name, and DEL in a header value
	MalformedCase{
		input: 'GET / HTTP/1.1\r\nBad\x01: v\r\n\r\n'
		err:   'error parsing request: invalid character in header "1"'
	},
	MalformedCase{
		input: 'GET / HTTP/1.1\r\nHost: a\x7fb\r\n\r\n'
		err:   'error parsing request: expecting "\r\n" after header'
	},
]

fn test_parse_request_malformed_table() {
	for c in malformed_cases {
		mut r := Request{}
		mut got_err := ''
		mut n := r.parse_request(c.input) or {
			got_err = err.msg()
			-99
		}
		assert got_err == c.err, 'input "${c.input}": error "${got_err}", want "${c.err}"'
		if got_err != '' {
			continue
		}
		assert n == c.ret, 'input "${c.input}": ret ${n}, want ${c.ret}'
		assert r.num_headers == c.num_headers, 'input "${c.input}": num_headers ${r.num_headers}, want ${c.num_headers}'
	}
}

fn test_parse_request_keeps_trailers_after_the_first_request() {
	// A second request in the same buffer is body, not headers.
	mut r := Request{}
	mut got_err := ''
	n := r.parse_request('GET / HTTP/1.1\r\nA: b\r\n\r\nC: d\r\n\r\n') or {
		got_err = err.msg()
		-99
	}
	assert got_err == '', got_err
	assert n == 24, 'ret ${n}'
	assert r.num_headers == 1, 'num_headers ${r.num_headers}'
	assert r.headers[0].name == 'A'
	assert r.headers[0].value == 'b'
	assert r.body == 'C: d\r\n\r\n', 'body "${r.body}"'
}

fn test_parse_request_trims_header_value_whitespace() {
	// leading OWS after the colon and trailing OWS before CRLF are both dropped
	mut r := Request{}
	n := r.parse_request('GET / HTTP/1.1\r\nHost:   x  \r\n\r\n') or {
		assert false, 'unexpected error: ${err}'
		0
	}
	assert n == 31, 'ret ${n}'
	assert r.headers[0].name == 'Host'
	assert r.headers[0].value == 'x', 'value "${r.headers[0].value}"'

	mut r2 := Request{}
	n2 := r2.parse_request('GET / HTTP/1.1\r\nHost:\tx\r\n\r\n') or {
		assert false, 'unexpected error: ${err}'
		0
	}
	assert n2 == 27, 'ret ${n2}'
	assert r2.headers[0].value == 'x', 'value "${r2.headers[0].value}"'
}

fn test_parse_request_allows_an_empty_header_value() {
	mut r := Request{}
	n := r.parse_request('GET / HTTP/1.1\r\nHost:\r\n\r\n') or {
		assert false, 'unexpected error: ${err}'
		0
	}
	assert n == 23, 'ret ${n}'
	assert r.num_headers == 1, 'num_headers ${r.num_headers}'
	assert r.headers[0].name == 'Host'
	assert r.headers[0].value == '', 'value "${r.headers[0].value}"'
}

fn test_parse_request_stores_obs_fold_as_a_nameless_header() {
	// A continuation line becomes a header with an empty name and the original
	// whitespace kept in the value.
	mut r := Request{}
	n := r.parse_request('GET / HTTP/1.1\r\nHost: x\r\n  y\r\n\r\n') or {
		assert false, 'unexpected error: ${err}'
		0
	}
	assert n == 32, 'ret ${n}'
	assert r.num_headers == 2, 'num_headers ${r.num_headers}'
	assert r.headers[0].name == 'Host'
	assert r.headers[0].value == 'x'
	assert r.headers[1].name == '', 'name "${r.headers[1].name}"'
	assert r.headers[1].value == '  y', 'value "${r.headers[1].value}"'

	mut r2 := Request{}
	n2 := r2.parse_request('GET / HTTP/1.1\r\nHost: x\r\n\ty\r\n\r\n') or {
		assert false, 'unexpected error: ${err}'
		0
	}
	assert n2 == 31, 'ret ${n2}'
	assert r2.num_headers == 2, 'num_headers ${r2.num_headers}'
	assert r2.headers[1].name == ''
	assert r2.headers[1].value == '\ty', 'value "${r2.headers[1].value}"'
}

fn test_parse_request_leading_crlf_is_accepted() {
	input := '\r\nGET / HTTP/1.1\r\nHost: x\r\n\r\n'
	mut r := Request{}
	assert r.parse_request(input)! == input.len
	assert r.method == 'GET'
	assert r.path == '/'
	assert r.num_headers == 1
	assert r.headers[0].name == 'Host'
	assert r.headers[0].value == 'x'
}

fn test_parse_request_leading_crlf_without_request_is_incomplete() {
	mut r := Request{}
	assert r.parse_request('\r\n')! == -2
}

fn test_parse_request_leading_bare_lf_is_rejected() {
	mut r := Request{}
	if _ := r.parse_request('\n') {
		assert false, 'a bare leading LF must be rejected'
	} else {
		assert err.msg() == 'error parsing request: invalid character "10"'
	}
}
