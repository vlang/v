// vtest build: present_openssl? && !sanitized_job? && !(windows && tinyc)
// vtest vflags: -d use_openssl
import net.http
import net.openssl
import os
import veb

const https_port = 13014

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
		ssl_config:         openssl.SSLConnectConfig{
			cert:     cert_path
			cert_key: key_path
		}
	)
	_ := <-app.started
	res := http.fetch(
		url:      'https://127.0.0.1:${https_port}/'
		validate: false
	)!
	assert res.status_code == 200
	assert res.body == 'secure'
}
