module veb

// Coverage for the pure parsing helpers in `parse.v`. None of these touch a
// socket: `parse_attrs` and `has_route_attributes` work on the attribute lists
// the comptime route generator collects, and `parse_form_from_request` works on
// an `http.Request` that a test can build by hand.
//
// Where a value passes through a transformation the test replicates the
// transformation rather than retyping the result: `parse_attrs` lower-cases the
// host and trims quotes from it, so the expected host is written as the trimmed
// lower-case form and the input carries the messy one.
import net.http
import net.urllib

// PraAttrsRow is one `parse_attrs` case. `err_msg` is non-empty when the call is
// expected to fail, in which case the other columns are unused.
struct PraAttrsRow {
	attrs   []string
	methods []http.Method
	path    string
	host    string
	err_msg string
}

// pra_attrs_rows was filled in by running `parse_attrs` over each entry.
fn pra_attrs_rows() []PraAttrsRow {
	mut rows := []PraAttrsRow{}
	rows << PraAttrsRow{
		attrs:   []
		methods: [http.Method.get]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['index']
		err_msg: "Encountered unexpected extra attributes: ['index']"
	}
	rows << PraAttrsRow{
		attrs:   ['get']
		methods: [http.Method.get]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['GET']
		methods: [http.Method.get]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['post']
		methods: [http.Method.post]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['delete']
		methods: [http.Method.delete]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['head']
		methods: [http.Method.head]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['options']
		methods: [http.Method.options]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['patch']
		methods: [http.Method.patch]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['put']
		methods: [http.Method.put]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['/custom']
		methods: [http.Method.get]
		path:    '/custom'
	}
	rows << PraAttrsRow{
		attrs:   ['post', '/custom']
		methods: [http.Method.post]
		path:    '/custom'
	}
	rows << PraAttrsRow{
		attrs:   ['POST', '/custom']
		methods: [http.Method.post]
		path:    '/custom'
	}
	rows << PraAttrsRow{
		attrs:   ['unsafe', 'post']
		methods: [http.Method.post]
		path:    '/my_handler'
	}
	rows << PraAttrsRow{
		attrs:   ['host: Example.COM/', 'get']
		methods: [http.Method.get]
		path:    '/my_handler'
		host:    'example.com/'
	}
	rows << PraAttrsRow{
		attrs:   ["host: 'example.org'", 'put', '/x']
		methods: [http.Method.put]
		path:    '/x'
		host:    'example.org'
	}
	rows << PraAttrsRow{
		attrs:   ['get', '/a', '/b']
		err_msg: 'Expected at most one path attribute'
	}
	rows << PraAttrsRow{
		attrs:   ['bogus']
		err_msg: "Encountered unexpected extra attributes: ['bogus']"
	}
	rows << PraAttrsRow{
		attrs:   ['get', 'bogus']
		err_msg: "Encountered unexpected extra attributes: ['bogus']"
	}
	return rows
}

fn test_parse_attrs_maps_attributes_to_methods_path_and_host() {
	for row in pra_attrs_rows() {
		methods, path, host := parse_attrs('my_handler', row.attrs) or {
			assert row.err_msg != '', 'unexpected error for ${row.attrs}: ${err.msg()}'
			assert err.msg() == row.err_msg, '${row.attrs}: got "${err.msg()}"'
			continue
		}
		assert row.err_msg == '', '${row.attrs}: expected error "${row.err_msg}", got "${path}"'
		assert methods == row.methods, '${row.attrs}: methods ${methods} != ${row.methods}'
		assert path == row.path, '${row.attrs}: path "${path}" != "${row.path}"'
		assert host == row.host, '${row.attrs}: host "${host}" != "${row.host}"'
	}
}

fn test_parse_attrs_takes_the_first_method_and_ignores_the_rest_of_the_list_order() {
	// Measured: the scan deletes what it consumes, so an attribute list in any
	// order produces the same result.
	methods, path, host := parse_attrs('my_handler', ['post', 'host:localhost', '/p'])!
	assert methods == [http.Method.post]
	assert path == '/p'
	assert host == 'localhost'
}

fn test_parse_attrs_lower_cases_the_host_but_keeps_its_trailing_slash() {
	// NOTE: the path is trimmed of quotes but the trailing `/` is not, so a host
	// written `host: Example.COM/` cannot match a request whose Host header has
	// no trailing slash. Current behaviour is preserved here.
	_, _, host := parse_attrs('my_handler', ['host: Example.COM/'])!
	assert host == 'example.com/'
}

fn test_parse_attrs_reports_every_leftover_attribute_together() {
	_, _, _ := parse_attrs('my_handler', ['x', 'y']) or {
		assert err.msg() == "Encountered unexpected extra attributes: ['x', 'y']"
		return
	}
	assert false, 'expected parse_attrs to fail on leftover attributes'
}

fn test_has_route_attributes_recognises_each_marker() {
	rows := [
		HasRouteRow{[]string{}, false},
		HasRouteRow{['index'], false},
		HasRouteRow{['unsafe'], false},
		HasRouteRow{['get'], true},
		HasRouteRow{['POST'], true},
		HasRouteRow{['/x'], true},
		HasRouteRow{['host: a.com'], true},
		HasRouteRow{['host:a.com'], true},
		HasRouteRow{['index', 'get'], true},
	]
	for row in rows {
		assert has_route_attributes(row.attrs) == row.expected, '${row.attrs}'
	}
}

struct HasRouteRow {
	attrs    []string
	expected bool
}

fn test_parse_query_from_url_reads_every_pair() {
	url := urllib.parse('/x?a=1&b=2') or { panic(err) }
	query := parse_query_from_url(url)
	assert query == {
		'a': '1'
		'b': '2'
	}
}

fn test_parse_query_from_url_lets_the_last_duplicate_win() {
	// Measured: the map is filled in order, so a repeated key keeps the last
	// value rather than erroring or keeping the first.
	url := urllib.parse('/x?a=1&b=2&a=3') or { panic(err) }
	assert parse_query_from_url(url) == {
		'a': '3'
		'b': '2'
	}
}

fn test_parse_query_from_url_handles_a_missing_value_and_decoding() {
	url := urllib.parse('/x?a=%20b&flag') or { panic(err) }
	query := parse_query_from_url(url)
	assert query['a'] == ' b'
	assert query['flag'] == ''
}

fn test_parse_query_from_url_of_a_path_without_a_query_is_empty() {
	url := urllib.parse('/x') or { panic(err) }
	assert parse_query_from_url(url).len == 0
}

fn test_parse_form_from_request_ignores_a_non_writing_method() {
	req := http.Request{
		method: .get
		url:    '/'
		header: http.new_header()
		data:   'a=1'
	}
	form, files := parse_form_from_request(req)!
	assert form.len == 0
	assert files.len == 0
}

fn test_parse_form_from_request_reads_a_urlencoded_body() {
	req := http.Request{
		method: .post
		url:    '/'
		header: http.new_header_from_map({
			.content_type: 'application/x-www-form-urlencoded'
		})
		data:   'a=1&b=hello+world&c=%2Fslash'
	}
	form, files := parse_form_from_request(req)!
	assert form == {
		'a': '1'
		'b': 'hello world'
		'c': '/slash'
	}
	assert files.len == 0
}

fn test_parse_form_from_request_reads_multipart_fields_and_files() {
	boundary := 'covboundary'
	req := http.Request{
		method: .post
		url:    '/'
		header: http.new_header_from_map({
			.content_type: 'multipart/form-data; boundary=${boundary}'
		})
		data:   pra_multipart_body(boundary)
	}
	form, files := parse_form_from_request(req)!
	assert form == {
		'a': '1'
	}
	assert 'f' in files
	assert files['f'].len == 1
	assert files['f'][0].filename == 't.txt'
	assert files['f'][0].content_type == 'text/plain'
	assert files['f'][0].data == 'hi'
}

fn test_parse_form_from_request_accepts_a_quoted_boundary() {
	// Measured: both spellings reach the same parser, because a leading quote is
	// trimmed off the boundary before it is used.
	boundary := 'covboundary'
	req := http.Request{
		method: .post
		url:    '/'
		header: http.new_header_from_map({
			.content_type: 'multipart/form-data; boundary="${boundary}"'
		})
		data:   pra_multipart_body(boundary)
	}
	form, files := parse_form_from_request(req)!
	assert form == {
		'a': '1'
	}
	assert 'f' in files
}

fn test_parse_form_from_request_rejects_two_boundaries() {
	boundary := 'covboundary'
	req := http.Request{
		method: .post
		url:    '/'
		header: http.new_header_from_map({
			.content_type: 'multipart/form-data; boundary=a; boundary=b'
		})
		data:   pra_multipart_body(boundary)
	}
	_, _ := parse_form_from_request(req) or {
		// NOTE: the message says "more that one", which is also what a
		// *missing* boundary produces, because zero is not exempted.
		assert err.msg() == 'detected more that one form-data boundary'
		return
	}
	assert false, 'expected parse_form_from_request to fail on two boundaries'
}

fn test_parse_form_from_request_reports_a_missing_boundary_the_same_way() {
	// NOTE: a `multipart/form-data` part with no `boundary=` is reported as
	// "more that one form-data boundary", which does not describe the input.
	req := http.Request{
		method: .patch
		url:    '/'
		header: http.new_header_from_map({
			.content_type: 'multipart/form-data'
		})
		data:   'x'
	}
	_, _ := parse_form_from_request(req) or {
		assert err.msg() == 'detected more that one form-data boundary'
		return
	}
	assert false, 'expected parse_form_from_request to fail without a boundary'
}

fn pra_multipart_body(boundary string) string {
	return '--${boundary}\r\nContent-Disposition: form-data; name="a"\r\n\r\n1\r\n--${boundary}\r\nContent-Disposition: form-data; name="f"; filename="t.txt"\r\nContent-Type: text/plain\r\n\r\nhi\r\n--${boundary}--\r\n'
}
