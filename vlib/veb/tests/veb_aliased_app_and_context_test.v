import os
import veb
import time
import net.http

const app_port = 29123
const exit_after = 10 * time.second

pub struct Context {
	veb.Context
}

pub struct App {
	veb.Middleware[Context]
	veb.Controller
	veb.StaticHandler
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

type AliasApp = App
type AliasContext = Context

fn testsuite_begin() {
	os.chdir(os.dir(@FILE))!
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

fn test_aliased_app_compiles_and_starts() {
	mut app := &AliasApp{}
	spawn veb.run_at[AliasApp, AliasContext](mut app,
		port:               app_port
		timeout_in_seconds: 2
		family:             .ip
	)
	eprintln('waiting for app to start ...')
	_ := <-app.started
	eprintln('>>> app was started')
}

fn test_static_root() {
	x := http.get('http://127.0.0.1:${app_port}/')!
	eprintln('>>>> http request was sent and received')
	assert x.status() == .not_found
	assert x.body == '404 Not Found'
}
