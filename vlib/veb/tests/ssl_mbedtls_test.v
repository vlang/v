// vtest build: !sanitized_job? && !use_openssl?
import net.http
import net.mbedtls
import os
import time
import veb

const https_port = 13013

pub struct Context {
	veb.Context
}

pub struct App {
	veb.Middleware[Context]
	started chan bool
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
		ssl_config:         mbedtls.SSLConnectConfig{
			cert:     cert_path
			cert_key: key_path
		}
	)
	select {
		_ := <-app.started {
		}
		5 * time.second {
			return error('mbedTLS HTTPS server did not start in time')
		}
	}
	res := http.fetch(
		url:      'https://127.0.0.1:${https_port}/'
		validate: false
	)!
	assert res.status_code == 200
	assert res.body == 'secure'
}

fn test_veb_answers_431_to_an_https_request_with_too_many_header_fields() ! {
	// net.http sends its own Host, User-Agent and Content-Length fields too
	mut header := http.new_header()
	for i in 0 .. http.max_headers {
		header.add_custom('X-Field-${i}', 'v')!
	}
	rejected := http.fetch(
		url:                      'https://127.0.0.1:${https_port}/'
		validate:                 false
		header:                   header
		disable_connection_reuse: true
	)!
	assert rejected.status_code == 431
	// the next client is still answered
	res := http.fetch(
		url:                      'https://127.0.0.1:${https_port}/'
		validate:                 false
		disable_connection_reuse: true
	)!
	assert res.status_code == 200
	assert res.body == 'secure'
}
