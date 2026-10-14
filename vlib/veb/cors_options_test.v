module veb

import net.http

const cov_cors_origin = 'https://cov.example'
const cov_cors_foreign_origin = 'https://foreign.example'

// cors_ctx builds a Context holding only the given request headers, so the CORS
// option methods can be driven without a server. `set_headers` writes to
// `ctx.res.header` and `validate_request` writes a body, both of which are
// readable afterwards.
fn cors_ctx(method http.Method, hdrs map[http.CommonHeader]string) Context {
	return Context{
		req:   http.Request{
			method: method
			url:    '/'
			header: http.new_header_from_map(hdrs)
		}
		query: {}
		form:  {}
		files: {}
		res:   http.Response{
			header: http.new_header()
		}
	}
}

fn assert_header(header http.Header, name http.CommonHeader, want string) {
	got := header.get(name) or {
		assert false, '${name} is missing, want "${want}"'
		return
	}
	assert got == want, '${name}: got "${got}", want "${want}"'
}

fn assert_header_absent(header http.Header, name http.CommonHeader) {
	if v := header.get(name) {
		assert false, '${name} should be absent, got "${v}"'
	}
}

fn test_cors_set_headers_wildcard_origin_echoes_the_caller_origin() {
	mut ctx := cors_ctx(.options, {
		.origin: cov_cors_foreign_origin
	})
	options := CorsOptions{
		origins:         ['*']
		allowed_methods: [.get, .post]
	}
	options.set_headers(mut ctx)
	assert_header(ctx.res.header, .access_control_allow_origin, cov_cors_foreign_origin)
	assert_header(ctx.res.header, .vary, 'Origin, Access-Control-Request-Headers')
	assert_header(ctx.res.header, .access_control_allow_methods, 'GET, POST')
	assert_header_absent(ctx.res.header, .access_control_allow_credentials)
	assert ctx.res.status_code == 0
	assert ctx.res.body == ''
}

fn test_cors_set_headers_emits_every_configured_header() {
	mut ctx := cors_ctx(.options, {
		.origin: cov_cors_origin
	})
	options := CorsOptions{
		origins:           ['*']
		allowed_methods:   [.get, .post]
		allowed_headers:   ['X-Token', 'X-Other']
		expose_headers:    ['X-Secret']
		max_age:           600
		allow_credentials: true
	}
	options.set_headers(mut ctx)
	assert_header(ctx.res.header, .access_control_allow_credentials, 'true')
	assert_header(ctx.res.header, .access_control_allow_headers, 'X-Token,X-Other')
	assert_header(ctx.res.header, .access_control_expose_headers, 'X-Secret')
	assert_header(ctx.res.header, .access_control_max_age, '600')
}

fn test_cors_set_headers_falls_back_to_the_safelisted_list() {
	mut ctx := cors_ctx(.options, {
		.origin:                         cov_cors_origin
		.access_control_request_headers: 'x-token'
	})
	options := CorsOptions{
		origins: [cov_cors_origin]
	}
	options.set_headers(mut ctx)
	assert_header(ctx.res.header, .access_control_allow_headers, cors_safelisted_response_headers)
}

fn test_cors_set_headers_omits_allow_headers_without_the_request_header() {
	mut ctx := cors_ctx(.options, {
		.origin: cov_cors_origin
	})
	options := CorsOptions{
		origins: [cov_cors_origin]
	}
	options.set_headers(mut ctx)
	assert_header_absent(ctx.res.header, .access_control_allow_headers)
	assert_header_absent(ctx.res.header, .access_control_allow_methods)
	assert_header_absent(ctx.res.header, .access_control_max_age)
}

fn test_cors_set_headers_skips_an_unlisted_origin() {
	mut ctx := cors_ctx(.options, {
		.origin: cov_cors_foreign_origin
	})
	options := CorsOptions{
		origins:         [cov_cors_origin]
		allowed_methods: [.get]
	}
	options.set_headers(mut ctx)
	assert ctx.res.header.keys().len == 0
}

fn test_cors_set_headers_skips_a_request_without_an_origin() {
	mut ctx := cors_ctx(.options, {})
	options := CorsOptions{
		origins:         ['*']
		allowed_methods: [.get]
	}
	options.set_headers(mut ctx)
	assert ctx.res.header.keys().len == 0
}

fn test_cors_validate_request_accepts_a_listed_origin_and_method() {
	mut ctx := cors_ctx(.get, {
		.origin: cov_cors_origin
	})
	options := CorsOptions{
		origins:         [cov_cors_origin]
		allowed_methods: [.get, .put]
	}
	assert options.validate_request(mut ctx)
	assert_header(ctx.res.header, .access_control_allow_origin, cov_cors_origin)
	assert_header(ctx.res.header, .vary, 'Origin, Access-Control-Request-Headers')
	assert ctx.res.status_code == 0
}

fn test_cors_validate_request_accepts_a_request_without_an_origin() {
	mut ctx := cors_ctx(.delete, {})
	options := CorsOptions{
		origins:         [cov_cors_origin]
		allowed_methods: [.get]
	}
	assert options.validate_request(mut ctx)
	assert ctx.res.header.keys().len == 0
}

fn test_cors_validate_request_rejects_an_unlisted_origin() {
	mut ctx := cors_ctx(.get, {
		.origin: cov_cors_foreign_origin
	})
	options := CorsOptions{
		origins:         [cov_cors_origin]
		allowed_methods: [.get]
	}
	assert !options.validate_request(mut ctx)
	assert ctx.res.status_code == int(http.Status.forbidden)
	assert ctx.res.body == 'invalid CORS origin'
	assert_header(ctx.res.header, .content_type, 'text/plain')
}

fn test_cors_validate_request_rejects_an_unlisted_method() {
	mut ctx := cors_ctx(.post, {
		.origin: cov_cors_origin
	})
	options := CorsOptions{
		origins:         [cov_cors_origin]
		allowed_methods: [.get, .put]
	}
	assert !options.validate_request(mut ctx)
	assert ctx.res.status_code == int(http.Status.method_not_allowed)
	assert ctx.res.body == 'POST requests are not allowed'
	assert_header(ctx.res.header, .access_control_allow_origin, cov_cors_origin)
}

// An empty `allowed_methods` matches nothing, so every request that carries an
// allowed origin is refused with a 405 rather than being waved through.
fn test_cors_validate_request_rejects_every_method_when_none_are_listed() {
	mut ctx := cors_ctx(.delete, {
		.origin: cov_cors_origin
	})
	options := CorsOptions{
		origins: [cov_cors_origin]
	}
	assert !options.validate_request(mut ctx)
	assert ctx.res.status_code == int(http.Status.method_not_allowed)
	assert ctx.res.body == 'DELETE requests are not allowed'
}

fn test_cors_validate_request_with_a_wildcard_header_allowlist_passes_any_header() {
	mut ctx := cors_ctx(.get, {
		.origin:     cov_cors_origin
		.user_agent: 'cov/1.0'
	})
	options := CorsOptions{
		origins:         [cov_cors_origin]
		allowed_methods: [.get]
		allowed_headers: ['*']
	}
	assert options.validate_request(mut ctx)
	assert ctx.res.status_code == 0
}

// NOTE: the allow-list check iterates every request header, and `Origin` is one
// of them, so a list that does not contain `Origin` refuses every request that
// carries one. That looks unintended, but it is what the code does, so it is
// asserted here rather than changed.
fn test_cors_validate_request_rejects_a_request_header_outside_the_allowlist() {
	mut ctx := cors_ctx(.get, {
		.origin:     cov_cors_origin
		.user_agent: 'cov/1.0'
	})
	options := CorsOptions{
		origins:         [cov_cors_origin]
		allowed_methods: [.get]
		allowed_headers: ['X-Token']
	}
	assert !options.validate_request(mut ctx)
	assert ctx.res.status_code == int(http.Status.forbidden)
	assert ctx.res.body == 'invalid Header "Origin"'
}

// NOTE: `cors_safelisted_response_headers` names CORS-safelisted *response*
// headers, and it is what a preflight is told are the allowed *request*
// headers. Asserted as it stands.
fn test_cors_safelisted_response_headers_holds_the_fetch_safelist() {
	assert cors_safelisted_response_headers == 'Cache-Control,Content-Language,Content-Length,Content-Type,Expires,Last-Modified,Pragma'
}
