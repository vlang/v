module veb

import net.http

// resp_ctx builds a Context whose request carries only the given headers. No
// socket is involved: `conn` stays nil and `client_fd` stays -1, which is the
// shape the response helpers have to tolerate when a handler runs without one.
fn resp_ctx(method http.Method, hdrs map[http.CommonHeader]string) Context {
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

fn test_user_agent_returns_the_request_header() {
	mut ctx := resp_ctx(.get, {
		.user_agent: 'cov/1.0'
	})
	assert ctx.user_agent() == 'cov/1.0'
}

fn test_user_agent_is_empty_without_the_header() {
	ctx := resp_ctx(.get, {})
	assert ctx.user_agent() == ''
}

fn test_get_custom_header_matches_the_name_in_either_case() {
	mut ctx := resp_ctx(.get, {})
	ctx.req.header.add_custom('X-Cov-Thing', 'yes') or { assert false, err.msg() }
	assert assert_custom(ctx, 'X-Cov-Thing') == 'yes'
	assert assert_custom(ctx, 'x-cov-thing') == 'yes'
}

fn assert_custom(ctx &Context, name string) string {
	return ctx.get_custom_header(name) or { '<absent>' }
}

fn test_get_custom_header_is_none_for_an_absent_name() {
	ctx := resp_ctx(.get, {})
	if v := ctx.get_custom_header('X-Cov-Absent') {
		assert false, 'an absent header must be none, got "${v}"'
	}
}

fn test_send_response_to_client_sets_the_usual_response_headers() {
	mut ctx := resp_ctx(.get, {})
	ctx.html('<b>hi</b>')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == '<b>hi</b>'
	assert ctx.res.http_version == '1.1'
	assert_header(ctx.res.header, .content_type, 'text/html')
	assert_header(ctx.res.header, .content_length, '9')
	assert_header(ctx.res.header, .server, 'veb')
	assert ctx.done
}

fn test_send_response_to_client_prefers_an_explicitly_set_content_type() {
	mut ctx := resp_ctx(.get, {})
	ctx.set_content_type('application/xml')
	ctx.text('x')
	assert_header(ctx.res.header, .content_type, 'application/xml')
	assert_header(ctx.res.header, .content_length, '1')
}

fn test_send_response_to_client_keeps_a_preset_content_length() {
	mut ctx := resp_ctx(.get, {})
	ctx.res.header.set(.content_length, '999')
	ctx.html('<b>hi</b>')
	assert_header(ctx.res.header, .content_length, '999')
	assert_header(ctx.res.header, .content_type, 'text/html')
}

fn test_send_response_to_client_sets_connection_close_only_for_a_closing_client() {
	mut closing := resp_ctx(.get, {})
	closing.client_wants_to_close = true
	closing.html('bye')
	assert_header(closing.res.header, .connection, 'close')

	mut keeping := resp_ctx(.get, {})
	keeping.html('bye')
	assert_header_absent(keeping.res.header, .connection)
}

fn test_send_response_to_client_leaves_no_content_type_for_an_empty_mimetype() {
	mut ctx := resp_ctx(.get, {})
	ctx.no_content()
	assert ctx.res.status_code == int(http.Status.no_content)
	assert ctx.res.body == ''
	assert_header(ctx.res.header, .content_length, '0')
	assert_header_absent(ctx.res.header, .content_type)
}

fn test_send_response_to_client_drops_a_second_response_over_one_connection() {
	mut ctx := resp_ctx(.get, {})
	ctx.html('one')
	ctx.html('two')
	assert ctx.res.body == 'one'
	assert_header(ctx.res.header, .content_length, '3')
	assert ctx.res.status_code == int(http.Status.ok)
}

fn test_server_error_sets_500_and_the_message_as_the_body() {
	mut ctx := resp_ctx(.get, {})
	ctx.server_error('boom')
	assert ctx.res.status_code == int(http.Status.internal_server_error)
	assert ctx.res.body == 'boom'
	assert_header(ctx.res.header, .content_type, 'text/plain')
	assert_header(ctx.res.header, .content_length, '4')
}

fn test_server_error_after_a_finished_response_leaves_the_first_body() {
	mut ctx := resp_ctx(.get, {})
	ctx.html('<b>hi</b>')
	ctx.server_error('boom')
	assert ctx.res.status_code == int(http.Status.internal_server_error)
	assert ctx.res.body == '<b>hi</b>'
	assert_header(ctx.res.header, .content_length, '9')
}

fn test_server_error_with_status_uses_the_status_it_is_given() {
	mut ctx := resp_ctx(.get, {})
	ctx.server_error_with_status(.not_found)
	assert ctx.res.status_code == int(http.Status.not_found)
	assert ctx.res.body == 'Server error'

	mut teapot := resp_ctx(.get, {})
	teapot.server_error_with_status(.im_a_teapot)
	assert teapot.res.status_code == int(http.Status.im_a_teapot)
}

fn test_time_to_render_is_zero_before_the_page_has_started() {
	ctx := resp_ctx(.get, {})
	assert ctx.time_to_render() == 0
}

fn test_time_to_render_counts_from_the_started_tick() {
	ctx := Context{
		req:            http.Request{
			header: http.new_header()
		}
		res:            http.Response{
			header: http.new_header()
		}
		page_gen_start: 1000
	}
	assert ctx.time_to_render() > 1000
}

fn test_takeover_conn_marks_the_context_and_leaves_the_response_unsent() {
	mut ctx := resp_ctx(.get, {})
	ctx.takeover_conn()
	assert ctx.takeover_mode == .manual
	assert ctx.conn == unsafe { nil }
	assert !ctx.done
	assert ctx.res.status_code == 0
}

fn test_takeover_conn_reusable_marks_the_context_as_reusable() {
	mut ctx := resp_ctx(.get, {})
	ctx.takeover_conn_reusable()
	assert ctx.takeover_mode == .reusable
	assert ctx.conn == unsafe { nil }
	assert !ctx.done
}
