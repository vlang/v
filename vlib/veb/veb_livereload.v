module veb

import net
import net.http
import os
import sync
import time

// Live reloading while developing. Compile your app with `-d veb_livereload`
// and run it under `v watch`:
//
// `v watch -d veb_livereload run .`
//
// Every HTML page served by veb then gets a small script before `</html>`.
// The script opens one Server-Sent Events stream to the server, and:
// * when `v watch` restarts the server (a .v file or template changed), it
//   fetches the current page again and morphs the DOM in place, so scroll
//   position, focus and typed form values survive;
// * when a file served by a `veb.StaticHandler` changes, the server pushes the
//   change without a rebuild: stylesheets are swapped in place, other assets reload the page.
//
// Compile with `-d veb_livereload_polling` as well to use the older polling
// script instead. Apps served over HTTPS need it: the stream is written to the
// taken-over TCP connection, which bypasses TLS. Proxies that block long-lived
// connections need it too.

// veb_livereload_server_start records when the veb server process started.
// Pages compare it with the id sent by the server, to detect a restart.
const veb_livereload_server_start = time.ticks().str()

// veb_livereload_poll_ms is the poll interval of the legacy polling script.
const veb_livereload_poll_ms = $d('veb_livereload_poll_ms', 250)

// veb_livereload_max_streams caps the number of open livereload streams.
const veb_livereload_max_streams = $d('veb_livereload_max_streams', 32)

// veb_livereload_retry_max_ms caps the delay between reconnect attempts while
// the app is down. Attempts start at 250 ms and double up to this cap, so the
// default retries every 250 ms: with `v watch` building the new version while
// the old one serves, the app is only down for the restart itself.
const veb_livereload_retry_max_ms = $d('veb_livereload_retry_max_ms', 250)

// veb_livereload_tick_ms is how often the watcher thread checks the build
// status written by `v watch`. It is short, so that a fast rebuild is still
// announced before `v watch` stops this server for the new one.
const veb_livereload_tick_ms = 50

// veb_livereload_asset_every is the number of ticks between static file checks (250 ms).
const veb_livereload_asset_every = 5

// veb_livereload_ping_every is the number of ticks between keepalive pings (15 s).
const veb_livereload_ping_every = 300

const veb_livereload_hub = &LivereloadHub{}

// LivereloadHub owns the open livereload streams of the process.
@[heap]
struct LivereloadHub {
mut:
	mu      &sync.Mutex = sync.new_mutex()
	conns   []&net.TcpConn
	started bool
	// status is the last build status written by `v watch`, see `livereload_watch`.
	status string
}

// veb_livereload_toast is '0' when compiled with `-d veb_livereload_no_toast`,
// which hides the status toast and the build error panel.
const veb_livereload_toast = $if veb_livereload_no_toast ? { '0' } $else { '1' }

// livereload_script_tag returns the tag injected before `</html>` in HTML responses.
fn livereload_script_tag(method string) string {
	return '<script src="/veb_livereload/${veb_livereload_server_start}/script.js" data-veb-livereload data-started="${veb_livereload_server_start}" data-method="${method}" data-retry-max="${veb_livereload_retry_max_ms}" data-toast="${veb_livereload_toast}"></script>'
}

// livereload_sse_frame formats one Server-Sent Events message. It delegates to
// the shared `http.SSEEvent` encoder, so livereload frames events exactly like
// every other SSE producer in vlib. Each line of `data` gets its own `data:`
// field; the browser joins them with newlines.
fn livereload_sse_frame(event string, data string) string {
	return http.SSEEvent{
		event: event
		data:  data
	}.encode()
}

// livereload_status_state returns the state on the first line of a build status.
fn livereload_status_state(status string) string {
	return status.all_before('\n').trim_space()
}

// livereload_stream_head is the HTTP response head of a livereload stream,
// followed by the `hello` message with the process id. It sends no `retry:`
// field: the client schedules its own reconnects, with backoff.
fn livereload_stream_head() string {
	return 'HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nCache-Control: no-cache\r\nConnection: keep-alive\r\nX-Accel-Buffering: no\r\n\r\n' +
		livereload_sse_frame('hello', veb_livereload_server_start)
}

// livereload_change_kind maps a changed static file to the change the page should apply.
fn livereload_change_kind(path string) string {
	return if os.file_ext(path).to_lower() == '.css' { 'css' } else { 'full' }
}

// livereload_script returns the client script for the current build.
fn livereload_script() string {
	$if veb_livereload_polling ? {
		return livereload_polling_script()
	} $else {
		return $embed_file('veb_livereload.js').to_string()
	}
}

// livereload_polling_script is the legacy client: it polls `/current` and reloads.
fn livereload_polling_script() string {
	return '"use strict";
function veb_livereload_checker_fn(started_at) {
	fetch("/veb_livereload/" + started_at + "/current", { cache: "no-cache" })
		.then((response) => response.text())
		.then(function (current_at) {
			if (started_at !== current_at) {
				window.location.reload();
			}
		});
}
const veb_livereload_checker = setInterval(veb_livereload_checker_fn, ${veb_livereload_poll_ms}, "${veb_livereload_server_start}");
'
}

// add registers a stream and sends it the response head and the `hello`
// message. It returns false, without writing, when the stream cap is reached.
fn (mut hub LivereloadHub) add(conn &net.TcpConn) bool {
	hub.mu.lock()
	defer {
		hub.mu.unlock()
	}
	if hub.conns.len >= veb_livereload_max_streams {
		return false
	}
	mut c := unsafe { conn }
	mut head := livereload_stream_head()
	if hub.status != '' && livereload_status_state(hub.status) != 'ok' {
		// A page that opens during a build, or after a failed one, shows it too.
		head += livereload_sse_frame('status', hub.status)
	}
	c.write_string(head) or {
		c.close() or {}
		return true
	}
	hub.conns << c
	return true
}

// set_status records a new build status and sends it to every open stream.
fn (mut hub LivereloadHub) set_status(status string) {
	hub.mu.lock()
	hub.status = status
	hub.mu.unlock()
	hub.broadcast('status', status)
}

// send writes `payload` to every open stream, and drops the streams that fail.
fn (mut hub LivereloadHub) send(payload string) {
	hub.mu.lock()
	mut alive := []&net.TcpConn{cap: hub.conns.len}
	for conn in hub.conns {
		mut c := unsafe { conn }
		c.write_string(payload) or {
			c.close() or {}
			continue
		}
		alive << c
	}
	hub.conns = alive
	hub.mu.unlock()
}

// broadcast sends one event to every open stream.
fn (mut hub LivereloadHub) broadcast(event string, data string) {
	hub.send(livereload_sse_frame(event, data))
}

// start runs the watcher thread once per process.
fn (mut hub LivereloadHub) start(static_files map[string]string) {
	hub.mu.lock()
	if hub.started {
		hub.mu.unlock()
		return
	}
	hub.started = true
	hub.mu.unlock()
	spawn livereload_watch(mut hub, static_files.values())
}

struct LivereloadFileState {
	mtime i64
	size  u64
}

fn livereload_file_state(path string) LivereloadFileState {
	st := os.stat(path) or { return LivereloadFileState{} }
	return LivereloadFileState{
		mtime: st.mtime
		size:  st.size
	}
}

// livereload_watch polls the static files for changes, pushes them to the
// pages, and pings idle streams so that closed tabs are noticed. When the app
// runs under `v watch`, it also follows the build status that `v watch` writes
// to the file named by `VEB_LIVERELOAD_STATUS`, and pushes each new status.
fn livereload_watch(mut hub LivereloadHub, paths []string) {
	mut states := map[string]LivereloadFileState{}
	for path in paths {
		states[path] = livereload_file_state(path)
	}
	status_path := os.getenv('VEB_LIVERELOAD_STATUS')
	mut last_status := ''
	if status_path != '' {
		last_status = os.read_file(status_path) or { '' }
		hub.mu.lock()
		hub.status = last_status
		hub.mu.unlock()
	}
	mut ticks := 0
	for {
		time.sleep(veb_livereload_tick_ms * time.millisecond)
		ticks++
		if status_path != '' {
			// The file is a few bytes (plus compiler output after a failed build):
			// compare its content, since file times only change once a second.
			status := os.read_file(status_path) or { '' }
			if status != '' && status != last_status {
				last_status = status
				hub.set_status(status)
			}
		}
		if ticks % veb_livereload_ping_every == 0 {
			hub.send(': ping\n\n')
		}
		if ticks % veb_livereload_asset_every != 0 {
			continue
		}
		mut kind := ''
		for path, old in states {
			now := livereload_file_state(path)
			if now == old {
				continue
			}
			states[path] = now
			if kind != 'full' {
				kind = livereload_change_kind(path)
			}
		}
		if kind != '' {
			hub.broadcast('change', kind)
		}
	}
}

// start_livereload starts the static file watcher, when compiled with `-d veb_livereload`.
@[if veb_livereload ?]
fn start_livereload(static_files map[string]string) {
	mut hub := unsafe { veb_livereload_hub }
	hub.start(static_files)
}

// handle_veb_livereload_current serves the process id, for the legacy polling script.
@[if veb_livereload ?]
fn (mut ctx Context) handle_veb_livereload_current() {
	ctx.send_response_to_client('text/plain', veb_livereload_server_start)
}

// handle_veb_livereload_script serves the client script.
@[if veb_livereload ?]
fn (mut ctx Context) handle_veb_livereload_script() {
	ctx.send_response_to_client('text/javascript', livereload_script())
}

// handle_veb_livereload_events takes over the connection and keeps it open as
// a Server-Sent Events stream; the hub writes to it from then on.
@[if veb_livereload ?]
fn (mut ctx Context) handle_veb_livereload_events() {
	mut hub := unsafe { veb_livereload_hub }
	ctx.takeover_conn()
	if ctx.conn == unsafe { nil } {
		return
	}
	mut conn := ctx.conn
	if !hub.add(conn) {
		conn.write_string('HTTP/1.1 503 Service Unavailable\r\nContent-Type: text/plain\r\nContent-Length: 27\r\nConnection: close\r\n\r\ntoo many livereload streams') or {}
		conn.close() or {}
	}
}
