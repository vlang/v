// vtest retry: 2
import os
import net
import net.http
import time

// Tests veb's live reload (`-d veb_livereload`) through a real server:
// the injected script tag, the Server-Sent Events stream, static file changes,
// the build status relayed from `v watch`, the polling fallback, the stream
// cap, and that nothing is added without the define.

const vexe = @VEXE
const tdir = os.join_path(os.vtmp_dir(), 'veb_livereload_test_${os.getpid()}')
const static_dir = os.join_path(tdir, 'static')
const status_path = os.join_path(tdir, 'status')
const server_source = os.join_path(@VMODROOT, 'vlib', 'veb', 'tests', 'livereload_test_server.v')
const base_port = 24000 + os.getpid() % 1000
const sse_port = base_port
const polling_port = base_port + 1
const plain_port = base_port + 2
const wait_timeout = 10 * time.second

struct Servers {
mut:
	procs []&os.Process
}

const servers = &Servers{}

fn testsuite_begin() {
	os.rmdir_all(tdir) or {}
	os.mkdir_all(static_dir)!
	os.write_file(os.join_path(static_dir, 'style.css'), 'body { color: black; }\n')!
	os.write_file(os.join_path(static_dir, 'app.js'), 'console.log(1);\n')!
	os.write_file(status_path, 'ok\n')!
	start_server('sse', ['-d', 'veb_livereload'], sse_port, true)
	start_server('polling', ['-d', 'veb_livereload', '-d', 'veb_livereload_polling', '-d',
		'veb_livereload_max_streams=1'], polling_port, false)
	start_server('plain', [], plain_port, false)
}

fn testsuite_end() {
	mut s := unsafe { servers }
	for mut p in s.procs {
		p.signal_kill()
		p.wait()
		p.close()
	}
	os.rmdir_all(tdir) or {}
}

fn start_server(name string, flags []string, port int, with_status bool) {
	exe := os.join_path(tdir, 'livereload_${name}.exe')
	res := os.exec([vexe, ...flags, '-o', exe, server_source])
	assert res.exit_code == 0, res.output
	mut p := os.new_process(exe)
	p.set_args([port.str(), static_dir])
	mut env := os.environ()
	if with_status {
		env['VEB_LIVERELOAD_STATUS'] = status_path
	}
	p.set_environment(env)
	p.set_redirect_stdio()
	p.run()
	mut s := unsafe { servers }
	s.procs << p
	stopwatch := time.new_stopwatch()
	for stopwatch.elapsed() < wait_timeout {
		if _ := http.get('http://127.0.0.1:${port}/plain') {
			return
		}
		time.sleep(50 * time.millisecond)
	}
	assert false, '${name} server did not start on port ${port}'
}

fn get(port int, path string) http.Response {
	return http.get('http://127.0.0.1:${port}${path}') or { panic(err) }
}

// open_stream opens a livereload stream, and returns it with what it sent first.
fn open_stream(port int) (&net.TcpConn, string) {
	return open_stream_until(port, '\n\n')
}

// open_stream_until opens a livereload stream, and reads until `needle`.
fn open_stream_until(port int, needle string) (&net.TcpConn, string) {
	mut conn := net.dial_tcp('127.0.0.1:${port}') or { panic(err) }
	conn.write_string('GET /veb_livereload/test/events HTTP/1.1\r\nHost: 127.0.0.1\r\nAccept: text/event-stream\r\n\r\n') or {
		panic(err)
	}
	head := read_until(mut conn, needle, '')
	return conn, head
}

// read_until reads from `conn` until `received` (starting with `prefix`) contains `needle`.
fn read_until(mut conn net.TcpConn, needle string, prefix string) string {
	conn.set_read_timeout(200 * time.millisecond)
	mut received := prefix
	mut buf := []u8{len: 4096}
	stopwatch := time.new_stopwatch()
	for !received.contains(needle) && stopwatch.elapsed() < wait_timeout {
		n := conn.read(mut buf) or { 0 }
		if n > 0 {
			received += buf[..n].bytestr()
		}
	}
	return received
}

fn write_status(content string) {
	tmp := status_path + '.tmp'
	os.write_file(tmp, content) or { panic(err) }
	os.mv(tmp, status_path) or { panic(err) }
}

fn page_server_id() string {
	page := get(sse_port, '/').body
	return page.all_after('data-started="').all_before('"')
}

fn test_html_pages_get_the_script_tag() {
	page := get(sse_port, '/').body
	assert page.contains('<script src="/veb_livereload/'), page
	assert page.contains(' data-veb-livereload '), page
	assert page.contains('data-method="GET"'), page
	assert page.ends_with('</script>\n</html>'), page
	assert !get(sse_port, '/plain').body.contains('veb_livereload')
	posted := http.post('http://127.0.0.1:${sse_port}/form', '') or { panic(err) }
	assert posted.body.contains('data-method="POST"'), posted.body
}

fn test_script_uses_server_sent_events() {
	res := get(sse_port, '/veb_livereload/test/script.js')
	assert res.status() == .ok
	assert (res.header.get(.content_type) or { '' }).starts_with('text/javascript')
	assert res.body.contains('new EventSource(')
	assert !res.body.contains('setInterval(')
	assert get(sse_port, '/veb_livereload/test/current').status() == .not_found
}

fn test_stream_starts_with_hello_and_the_server_id() {
	mut conn, head := open_stream(sse_port)
	defer {
		conn.close() or {}
	}
	assert head.starts_with('HTTP/1.1 200 OK\r\n'), head
	assert head.contains('Content-Type: text/event-stream\r\n'), head
	assert head.contains('event: hello\ndata: ${page_server_id()}\n\n'), head
	assert !head.contains('retry:'), head
}

fn test_static_file_changes_are_pushed() {
	mut conn, _ := open_stream(sse_port)
	defer {
		conn.close() or {}
	}
	os.write_file(os.join_path(static_dir, 'style.css'), 'body { color: red; }\n')!
	got := read_until(mut conn, 'data: css', '')
	assert got.contains('event: change\ndata: css\n\n'), got
	os.write_file(os.join_path(static_dir, 'app.js'), 'console.log(22);\n')!
	got2 := read_until(mut conn, 'data: full', '')
	assert got2.contains('event: change\ndata: full\n\n'), got2
}

fn test_build_status_is_relayed() {
	mut conn, _ := open_stream(sse_port)
	defer {
		conn.close() or {}
	}
	write_status('building\n')
	got := read_until(mut conn, 'data: building', '')
	assert got.contains('event: status\ndata: building\n'), got
	write_status('failed\nmain.v:1:1: error: first\nmain.v:2:1: error: second')
	got2 := read_until(mut conn, 'error: second', '')
	assert got2.contains('event: status\ndata: failed\ndata: main.v:1:1: error: first\ndata: main.v:2:1: error: second\n\n'), got2
}

fn test_a_new_page_sees_a_failed_build() {
	// The status is still `failed` from the previous test.
	mut conn, head := open_stream(sse_port)
	got := read_until(mut conn, 'error: second', head)
	conn.close() or {}
	assert got.contains('event: hello'), got
	assert got.contains('event: status\ndata: failed\n'), got
	write_status('ok\n')
	time.sleep(200 * time.millisecond)
	mut conn2, head2 := open_stream(sse_port)
	conn2.close() or {}
	assert !head2.contains('event: status'), head2
}

fn test_polling_fallback() {
	script := get(polling_port, '/veb_livereload/test/script.js').body
	assert script.contains('setInterval(veb_livereload_checker_fn, 250,'), script
	assert !script.contains('EventSource')
	id := get(polling_port, '/').body.all_after('data-started="').all_before('"')
	assert get(polling_port, '/veb_livereload/test/current').body == id
}

fn test_stream_cap() {
	// The polling server was built with `-d veb_livereload_max_streams=1`.
	mut first, head := open_stream(polling_port)
	defer {
		first.close() or {}
	}
	assert head.starts_with('HTTP/1.1 200 OK'), head
	mut second, head2 := open_stream_until(polling_port, 'too many livereload streams')
	second.close() or {}
	assert head2.starts_with('HTTP/1.1 503 Service Unavailable'), head2
}

fn test_nothing_is_added_without_the_define() {
	page := get(plain_port, '/').body
	assert page == '<html><body><p id="greeting">hello</p></body></html>', page
	assert get(plain_port, '/veb_livereload/test/events').status() == .not_found
	assert get(plain_port, '/veb_livereload/test/script.js').status() == .not_found
}
