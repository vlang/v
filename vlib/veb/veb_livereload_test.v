module veb

fn test_sse_frame_single_line() {
	assert livereload_sse_frame('change', 'css') == 'event: change\ndata: css\n\n'
}

fn test_sse_frame_gives_each_line_its_own_data_field() {
	frame := livereload_sse_frame('status', 'failed\nfirst error\r\nsecond error')
	assert frame == 'event: status\ndata: failed\ndata: first error\ndata: second error\n\n'
}

fn test_status_state_is_the_first_line() {
	assert livereload_status_state('building\n') == 'building'
	assert livereload_status_state('failed\nsome.v:1:1: error: oops') == 'failed'
	assert livereload_status_state('ok') == 'ok'
	assert livereload_status_state('') == ''
}

fn test_change_kind() {
	assert livereload_change_kind('static/css/main.css') == 'css'
	assert livereload_change_kind('static/css/MAIN.CSS') == 'css'
	assert livereload_change_kind('static/js/app.js') == 'full'
	assert livereload_change_kind('static/img/logo.png') == 'full'
}

fn test_script_tag_carries_the_client_settings() {
	tag := livereload_script_tag('POST')
	assert tag.starts_with('<script src="/veb_livereload/${veb_livereload_server_start}/script.js"')
	assert tag.contains(' data-veb-livereload ')
	assert tag.contains('data-started="${veb_livereload_server_start}"')
	assert tag.contains('data-method="POST"')
	assert tag.contains('data-retry-max="${veb_livereload_retry_max_ms}"')
	assert tag.contains('data-toast="${veb_livereload_toast}"')
}

fn test_stream_head_sends_hello_and_no_retry_field() {
	head := livereload_stream_head()
	assert head.starts_with('HTTP/1.1 200 OK\r\n')
	assert head.contains('Content-Type: text/event-stream\r\n')
	assert head.contains('Cache-Control: no-cache\r\n')
	assert head.ends_with('\r\n\r\nevent: hello\ndata: ${veb_livereload_server_start}\n\n')
	assert !head.contains('retry:')
}

fn test_scripts() {
	assert livereload_script().contains('new EventSource(')
	polling := livereload_polling_script()
	assert polling.contains('setInterval(veb_livereload_checker_fn, ${veb_livereload_poll_ms}')
	assert polling.contains('"${veb_livereload_server_start}"')
}
