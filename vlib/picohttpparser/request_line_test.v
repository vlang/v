module picohttpparser

// Coverage for `parse_request_path` and `parse_request_path_pipeline`, the two
// entry points that stop at the request line instead of parsing the whole
// request. Every `ret`/`method`/`path`/`err` value below was measured on this
// tree, not derived from the RFC.

struct RequestLineCase {
	input  string
	ret    int
	method string
	path   string
	err    string
}

const request_line_cases = [
	RequestLineCase{
		input:  'GET /path HTTP/1.1\r\nHost: x\r\n\r\n'
		ret:    10
		method: 'GET'
		path:   '/path'
	},
	RequestLineCase{
		input:  'GET /path '
		ret:    10
		method: 'GET'
		path:   '/path'
	},
	RequestLineCase{
		input:  'GET  /a  HTTP/1.1\r\n\r\n'
		ret:    9
		method: 'GET'
		path:   '/a'
	},
	RequestLineCase{
		input:  'GET http://example.com/x HTTP/1.1\r\n\r\n'
		ret:    25
		method: 'GET'
		path:   'http://example.com/x'
	},
	RequestLineCase{
		input:  'GET /a/very/long/path?q=1&z=2 HTTP/1.1\r\n\r\n'
		ret:    30
		method: 'GET'
		path:   '/a/very/long/path?q=1&z=2'
	},
	// No space after the path: the version is read as the path token, which
	// then runs straight into the CR and is rejected as a control character.
	RequestLineCase{
		input:  'GET /path HTTP/1.1\r\n\r\n'
		ret:    10
		method: 'GET'
		path:   '/path'
	},
	RequestLineCase{
		input: 'GET HTTP/1.1\r\n\r\n'
		err:   'error parsing request: invalid character "13"'
	},
	RequestLineCase{
		input: 'GET /path'
		ret:   -2
	},
	RequestLineCase{
		input: 'GET'
		ret:   -2
	},
	RequestLineCase{
		input: ''
		err:   'error parsing request: invalid character "0"'
	},
	RequestLineCase{
		input: 'GET\t/a HTTP/1.1\r\n\r\n'
		err:   'error parsing request: invalid character "9"'
	},
	RequestLineCase{
		input: ' GET /a HTTP/1.1\r\n\r\n'
		err:   'error parsing request: invalid method or path'
	},
	RequestLineCase{
		input: '  HTTP/1.1\r\n\r\n'
		err:   'error parsing request: invalid character "13"'
	},
]

pub fn test_parse_request_path_table() {
	for c in request_line_cases {
		mut r := Request{}
		mut got_err := ''
		mut n := r.parse_request_path(c.input) or {
			got_err = err.msg()
			-99
		}
		assert got_err == c.err, 'input "${c.input}": error "${got_err}", want "${c.err}"'
		if got_err != '' {
			continue
		}
		assert n == c.ret, 'input "${c.input}": ret ${n}, want ${c.ret}'
		assert r.method == c.method, 'input "${c.input}": method "${r.method}", want "${c.method}"'
		assert r.path == c.path, 'input "${c.input}": path "${r.path}", want "${c.path}"'
	}
}

pub fn test_parse_request_path_rejects_control_characters() {
	for raw in ['GE\x01T /a HTTP/1.1\r\n\r\n', 'GET /a\x7fb HTTP/1.1\r\n\r\n'] {
		mut r := Request{}
		mut got_err := ''
		r.parse_request_path(raw) or { got_err = err.msg() }
		assert got_err != '', 'expected a parse error for "${raw}"'
		assert got_err.starts_with('error parsing request: invalid character "'), got_err
	}
}

pub fn test_pipeline_reads_requests_from_one_buffer() {
	mut r := Request{}
	buffer := 'GET /a HTTP/1.1\r\nHost: x\r\n\r\n' + 'GET /b HTTP/1.1\r\nHost: y\r\n\r\n'

	mut got_err := ''
	mut first := r.parse_request_path_pipeline(buffer) or {
		got_err = err.msg()
		-99
	}
	assert got_err == '', 'first call: ${got_err}'
	assert first == 28, 'first ret ${first}'
	assert r.prev_len == 28, 'first prev_len ${r.prev_len}'
	assert r.method == 'GET'
	assert r.path == '/a'

	mut second := r.parse_request_path_pipeline(buffer) or {
		got_err = err.msg()
		-99
	}
	assert got_err == '', 'second call: ${got_err}'
	assert second == 28, 'second ret ${second}'
	assert r.method == 'GET'
	assert r.path == '/b'

	// NOTE: prev_len is assigned the offset *relative to the previous
	// prev_len*, so it is 28 again rather than 56. A third call therefore
	// re-parses the second request instead of advancing.
	mut third := r.parse_request_path_pipeline(buffer) or {
		got_err = err.msg()
		-99
	}
	assert got_err == '', 'third call: ${got_err}'
	assert third == 28, 'third ret ${third}'
	assert r.prev_len == 28, 'third prev_len ${r.prev_len}'
	assert r.path == '/b'
}

pub fn test_pipeline_single_request_sets_prev_len() {
	mut r := Request{}
	mut got_err := ''
	n := r.parse_request_path_pipeline('GET /a HTTP/1.1\r\nHost: x\r\n\r\n') or {
		got_err = err.msg()
		-99
	}
	assert got_err == '', got_err
	assert n == 28, 'ret ${n}'
	assert r.prev_len == 28, 'prev_len ${r.prev_len}'
	assert r.method == 'GET'
	assert r.path == '/a'
}

pub fn test_pipeline_without_final_blank_line_is_rejected() {
	mut r := Request{}
	mut got_err := ''
	n := r.parse_request_path_pipeline('GET /a HTTP/1.1\r\nHost: x\r\n') or {
		got_err = err.msg()
		-99
	}
	assert n == -99, 'ret ${n}'
	assert got_err == 'error parsing request: no request found', got_err
	assert r.prev_len == 0, 'prev_len ${r.prev_len}'
	assert r.method == 'GET'
	assert r.path == '/a'
}

pub fn test_pipeline_empty_input_has_no_request() {
	mut r := Request{}
	mut got_err := ''
	n := r.parse_request_path_pipeline('') or {
		got_err = err.msg()
		-99
	}
	assert n == -99, 'ret ${n}'
	assert got_err == 'error parsing request: no request found', got_err
	assert r.prev_len == 0, 'prev_len ${r.prev_len}'
	// NOTE: `advance_token2` has no end-of-buffer check, so `method` and `path`
	// hold bytes read past the empty buffer. They are deliberately not asserted.
}
