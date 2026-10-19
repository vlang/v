// json_form_test.v - a JSON body fills the same form map a urlencoded body does.
//
// `parse_form` used to glob-match anything that looked like `{...}` and file the
// raw text under a literal `json` key. Two cases follow from that: a handler
// receiving `{"name":"x"}` saw `ctx.form['json'] == '{"name":"x"}'`, and an
// object body with a single trailing space never matched the glob, so it went
// to the urlencoded path, produced no keys at all, and the body vanished while
// the handler still answered 200.
//
// These go through `render_request` rather than a listener, so what is asserted
// is what a route handler actually receives.
module veb

import net.http

struct JsonApp {}

struct JsonContext {
	Context
}

@['/echo'; post; put; patch]
fn (app &JsonApp) echo(mut ctx JsonContext) Result {
	return ctx.text(ctx.form.str())
}

// json_request posts one body with one content type through the dispatcher.
fn json_request(url string, body string, content_type string) &Context {
	return json_method_request(.post, url, body, content_type)
}

fn json_method_request(method http.Method, url string, body string, content_type string) &Context {
	mut app := JsonApp{}
	mut header := http.new_header_from_map({
		http.CommonHeader.host: 'localhost'
	})
	if content_type != '' {
		header.set(.content_type, content_type)
	}
	header.set(.content_length, '${body.len}')
	return render_request[JsonApp, JsonContext](mut app, http.Request{
		method: method
		url:    url
		data:   body
		header: header
	}) or { panic(err) }
}

fn test_a_json_object_body_fills_the_form_map() {
	// The behaviour being added. Before it this was `{'json': '{"name":"x","id":3}'}`.
	ctx := json_request('/echo', '{"name":"x","id":3}', 'application/json')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.form == {
		'name': 'x'
		'id':   '3'
	}
}

fn test_json_values_of_every_scalar_kind_are_read() {
	ctx := json_request('/echo', '{"s":"text","i":7,"f":1.5,"b":true}', 'application/json')
	assert ctx.form == {
		's': 'text'
		'i': '7'
		'f': '1.5'
		'b': 'true'
	}
}

fn test_a_json_object_with_a_trailing_space_is_not_dropped() {
	// The dangerous half of the old bug: the glob did not match, the urlencoded
	// path found no `=`, and the body was silently discarded with a 200.
	ctx := json_request('/echo', '{"name":"x"} ', 'application/json')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.form == {
		'name': 'x'
	}
	assert ctx.req.data == '{"name":"x"} '
}

fn test_a_charset_suffixed_json_content_type_is_recognised() {
	for content_type in [
		'application/json; charset=utf-8',
		'Application/JSON',
		'APPLICATION/JSON; charset=UTF-8',
		' \tApplication/Json \t; charset=utf-8',
	] {
		ctx := json_request('/echo', '{"name":"x"}', content_type)
		assert ctx.res.status_code == int(http.Status.ok), content_type
		assert ctx.form == {
			'name': 'x'
		}, content_type
	}
}

fn test_json_nested_values_and_null_fields_are_skipped() {
	body := '{"name":"x","object":{"name":"nested"},"array":[1,2],"empty":null}'
	ctx := json_request('/echo', body, 'application/json')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.form == {
		'name': 'x'
	}
	assert ctx.req.data == body
	assert ctx.files.len == 0
}

fn test_json_objects_with_surrounding_whitespace_are_accepted() {
	for body in [' \r\n\t{"name":"x"}\n\t ', ' \t{}\r\n'] {
		ctx := json_request('/echo', body, 'application/json')
		assert ctx.res.status_code == int(http.Status.ok)
		assert ctx.form == if body.contains('name') {
			{
				'name': 'x'
			}
		} else {
			map[string]string{}
		}
		assert ctx.req.data == body
	}
}

fn test_a_urlencoded_body_still_goes_through_the_urlencoded_path() {
	ctx := json_request('/echo', 'name=x&id=3', 'application/x-www-form-urlencoded')
	assert ctx.form == {
		'name': 'x'
		'id':   '3'
	}
}

fn test_json_is_decoded_for_post_put_and_patch() {
	for method in [http.Method.post, .put, .patch] {
		ctx := json_method_request(method, '/echo', '{"name":"x"}', 'application/json')
		assert ctx.res.status_code == int(http.Status.ok)
		assert ctx.form == {
			'name': 'x'
		}
	}
}

fn test_json_is_not_guessed_from_the_body() {
	for content_type in ['', 'text/plain', 'application/x-www-form-urlencoded'] {
		ctx := json_request('/echo', '{"name":"x"}', content_type)
		assert ctx.res.status_code == int(http.Status.ok)
		assert ctx.form.len == 0
		assert ctx.req.data == '{"name":"x"}'
	}
}

fn test_a_json_body_that_is_not_an_object_is_an_error() {
	for body in ['null', ' \r\nnull\t ', '[]', '[{"id":3}]', '"text"', '3', '1.5', 'true', 'false'] {
		ctx := json_request('/echo', body, 'application/json')
		assert ctx.res.status_code == int(http.Status.bad_request), body
		assert ctx.res.body.starts_with('Failed to parse form data:'), body
	}
}

fn test_malformed_json_is_an_error() {
	for body in ['', ' \r\n\t ', '{', '{"name":}', '{"name":"x"} trailing'] {
		ctx := json_request('/echo', body, 'application/json')
		assert ctx.res.status_code == int(http.Status.bad_request), body
	}
}

fn test_tls_json_parsing_uses_the_existing_bad_request_policy() ! {
	mut app := JsonApp{}
	routes := generate_routes[JsonApp, JsonContext](app)!
	params := &SslRequestParams{
		// The TLS dispatcher stores a type-erased pointer to this live application.
		global_app: unsafe { voidptr(&app) }
		routes:     &routes
	}
	for body in ['null', '[]', '3', '{'] {
		if _ := handle_ssl_request[JsonApp, JsonContext](http.Request{
			method: .post
			url:    '/echo'
			header: http.new_header(key: .content_type, value: 'application/json')
			data:   body
		}, params) {
			assert false, 'TLS must select its 400 response for ${body}'
		}
	}
	ctx := handle_ssl_request[JsonApp, JsonContext](http.Request{
		method: .post
		url:    '/echo'
		header: http.new_header(key: .content_type, value: 'Application/JSON; charset=utf-8')
		data:   '{"name":"x"}'
	}, params) or { panic('TLS rejected a JSON object') }
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.form == {
		'name': 'x'
	}
	assert ctx.req.data == '{"name":"x"}'
}
