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

@['/echo'; post]
fn (app &JsonApp) echo(mut ctx JsonContext) Result {
	return ctx.text(ctx.form.str())
}

// json_request posts one body with one content type through the dispatcher.
fn json_request(url string, body string, content_type string) &Context {
	mut app := JsonApp{}
	mut header := http.new_header_from_map({
		http.CommonHeader.host: 'localhost'
	})
	if content_type != '' {
		header.set(.content_type, content_type)
	}
	header.set(.content_length, '${body.len}')
	return render_request[JsonApp, JsonContext](mut app, http.Request{
		method: .post
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
}

fn test_a_charset_suffixed_json_content_type_is_recognised() {
	// Browsers and clients send `application/json; charset=utf-8`; veb splits the
	// header on `;` before matching.
	ctx := json_request('/echo', '{"name":"x"}', 'application/json; charset=utf-8')
	assert ctx.form == {
		'name': 'x'
	}
}

fn test_a_urlencoded_body_still_goes_through_the_urlencoded_path() {
	ctx := json_request('/echo', 'name=x&id=3', 'application/x-www-form-urlencoded')
	assert ctx.form == {
		'name': 'x'
		'id':   '3'
	}
}

fn test_a_json_body_that_is_not_an_object_is_an_error() {
	// A JSON array is now a clear failure rather than an empty form map, so a
	// handler that typedef its body is told instead of seeing nothing.
	ctx := json_request('/echo', '[{"id":3}]', 'application/json')
	assert ctx.res.status_code == int(http.Status.bad_request)
}
