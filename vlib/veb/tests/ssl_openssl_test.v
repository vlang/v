// vtest build: present_openssl? && !sanitized_job? && !(windows && tinyc)
// vtest vflags: -d use_openssl
import net.http
import net.openssl
import os
import time
import veb

const https_port = 13014

pub struct Context {
	veb.Context
}

pub struct App {
	veb.Middleware[Context]
	started  chan bool
	activity chan string = chan string{cap: 16}
}

// before_handle_request records entry into TLS dispatch.
pub fn (mut app App) before_handle_request(req http.Request) {
	app.activity <- 'before:' + req.url
}

// after_handle_request records completion of TLS dispatch.
pub fn (mut app App) after_handle_request(req http.Request) {
	app.activity <- 'after:' + req.url
}

fn passthrough_middleware(mut _ctx Context) bool {
	return true
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (app &App) index(mut ctx Context) veb.Result {
	return ctx.text('secure')
}

fn test_veb_serves_https_requests() ! {
	cert_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.crt')
	key_path := os.join_path(@VMODROOT, 'examples', 'ssl_server', 'cert', 'server.key')
	mut app := &App{}
	app.use(handler: passthrough_middleware)
	spawn veb.run_at[App, Context](mut app,
		host:               '127.0.0.1'
		port:               https_port
		family:             .ip
		timeout_in_seconds: 2
		ssl_config:         openssl.SSLConnectConfig{
			cert:     cert_path
			cert_key: key_path
		}
	)
	select {
		_ := <-app.started {
		}
		5 * time.second {
			return error('OpenSSL HTTPS server did not start in time')
		}
	}
	res := http.fetch(
		url:      'https://127.0.0.1:${https_port}/'
		validate: false
	)!
	assert res.status_code == 200
	assert res.body == 'secure'
	for expected in ['before:/', 'after:/'] {
		select {
			event := <-app.activity {
				assert event == expected
			}
			5 * time.second {
				return error('missing TLS activity hook: ${expected}')
			}
		}
	}
}

fn test_veb_answers_431_to_an_oversized_https_request_head() ! {
	path := '/' + 'a'.repeat(http.default_max_http1_request_head_size)
	res := http.fetch(
		url:                      'https://127.0.0.1:${https_port}${path}'
		validate:                 false
		disable_connection_reuse: true
	)!
	assert res.status_code == 431
}

fn test_veb_closes_a_slow_https_request_head() ! {
	mut slow := openssl.new_ssl_conn(openssl.SSLConnectConfig{})!
	defer { slow.shutdown() or {} }
	slow.dial('127.0.0.1', https_port)!
	slow.set_read_timeout(4 * time.second)
	slow.write_string('GET / HTTP/1.1\r\nX-Slow: ')!
	started := time.new_stopwatch()
	for _ in 0 .. 30 {
		time.sleep(100 * time.millisecond)
		slow.write_string('a') or { break }
	}
	mut response := []u8{}
	mut buffer := []u8{len: 1024}
	for {
		n := slow.read(mut buffer) or { break }
		if n <= 0 {
			break
		}
		response << buffer[..n]
		if response.bytestr().contains('\r\n\r\n') {
			break
		}
	}
	response_text := response.bytestr()
	assert response_text == '' || response_text.starts_with('HTTP/1.1 400 Bad Request\r\n'), response_text
	assert started.elapsed() < 2800 * time.millisecond, 'slow request closed after ${started.elapsed()}'
}
