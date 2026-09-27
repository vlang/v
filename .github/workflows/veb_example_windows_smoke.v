// veb_example_windows_smoke.v is a CI smoke test for the fasthttp based `veb`
// backend. It compiles `examples/veb/veb_example.v`, runs the produced server,
// and performs real HTTP GET requests to verify that the server actually
// accepts connections and returns the expected HTML.
//
// It is primarily a regression test for the Windows crash that happened when
// `accept()` truncated pointer-sized `SOCKET` handles (see
// vlib/fasthttp/fasthttp_windows.c.v), which made every veb/fasthttp server
// crash on the very first incoming request on Windows. The logic itself is
// cross platform, so the same program can be reused on other CI targets.
//
// Usage: v run .github/workflows/veb_example_windows_smoke.v
module main

import os
import time
import net.http

const vexe = @VEXE
const vroot = os.dir(vexe)
const port = 8082
const server_addr = '127.0.0.1:${port}'

// expected_index_snippets are stable fragments of the rendered index.html
// template (examples/veb/index.html + header.html + footer.html).
const expected_index_snippets = [
	'<title>veb example page</title>',
	'header from included header.html',
	'Hello world from veb, request number:',
	'End.',
	'footer',
]

fn main() {
	source := os.join_path(vroot, 'examples', 'veb', 'veb_example.v')
	if !os.exists(source) {
		fail('cannot find the veb example source: ${source}')
	}
	serverexe := os.join_path(os.vtmp_dir(), 'veb_example_smoke${dot_exe}')
	os.rm(serverexe) or {}

	// 1. Compile examples/veb/veb_example.v (veb uses the fasthttp backend).
	println('> compiling ${source} ...')
	compile :=
		os.execute('${os.quoted_path(vexe)} -o ${os.quoted_path(serverexe)} ${os.quoted_path(source)}')
	if compile.exit_code != 0 {
		fail('veb_example.v failed to compile:\n${compile.output}')
	}
	if !os.exists(serverexe) {
		fail('the veb_example server executable was not produced: ${serverexe}')
	}

	// 2. Start the compiled server in the background.
	println('> starting the veb server: ${serverexe}')
	mut p := os.new_process(serverexe)
	p.set_work_folder(vroot)
	p.run()

	// 3. Run the HTTP checks, then always shut the server down afterwards, so
	//    a failing assertion never leaks a listening server process.
	mut check_error := ''
	run_http_checks(mut p) or { check_error = err.msg() }
	if p.is_alive() {
		p.signal_kill()
	}
	p.wait()
	os.rm(serverexe) or {}

	if check_error != '' {
		fail(check_error)
	}
	println('veb example fasthttp smoke test: OK')
}

// run_http_checks waits for the freshly started `veb_example` server to accept
// connections, then verifies that it returns the expected HTML and text.
fn run_http_checks(mut p os.Process) ! {
	println('> waiting for the server to accept connections on ${server_addr} ...')
	mut index_body := ''
	mut is_up := false
	for _ in 0 .. 150 {
		if !p.is_alive() {
			return error('the veb server exited before answering any request (exit code ${p.code}); this indicates a crash')
		}
		resp := get_with_timeout('http://${server_addr}/') or {
			time.sleep(100 * time.millisecond)
			continue
		}
		if resp.status_code != 200 {
			return error('GET / returned status ${resp.status_code}, expected 200')
		}
		index_body = resp.body
		is_up = true
		break
	}
	if !is_up {
		return error('the server did not start accepting connections on ${server_addr} in time')
	}

	// GET / must return the rendered index.html template.
	for snippet in expected_index_snippets {
		if !index_body.contains(snippet) {
			return error('GET / response is missing the expected html snippet `${snippet}`.\nFull body:\n${index_body}')
		}
	}
	println('> GET / returned the expected html (${index_body.len} bytes)')

	// GET /show_text must return the plain text route.
	text_resp := get_with_timeout('http://${server_addr}/show_text')!
	if text_resp.status_code != 200 {
		return error('GET /show_text returned status ${text_resp.status_code}, expected 200')
	}
	if text_resp.body != 'Hello world from veb' {
		return error('GET /show_text returned `${text_resp.body}`, expected `Hello world from veb`')
	}
	println('> GET /show_text returned the expected text')
}

// get_with_timeout performs an HTTP GET with short timeouts, so that a
// half-crashed server that accepts a connection but never answers cannot stall
// the whole CI job.
fn get_with_timeout(url string) !http.Response {
	return http.fetch(
		url:           url
		method:        .get
		read_timeout:  5 * time.second
		write_timeout: 5 * time.second
	)
}

fn fail(msg string) {
	eprintln('veb example fasthttp smoke test FAILED: ${msg}')
	exit(1)
}

const dot_exe = $if windows { '.exe' } $else { '' }
