import net.http
import time
import veb

const catchall_route_order_port = 13062

struct CatchallRouteOrderContext {
	veb.Context
}

struct CatchallRouteOrderApp {
mut:
	server  &veb.Server = unsafe { nil }
	started chan bool
}

pub fn (mut app CatchallRouteOrderApp) init_server(server &veb.Server) {
	app.server = server
}

pub fn (mut app CatchallRouteOrderApp) before_accept_loop() {
	app.started <- true
}

@['/:path...']
fn (mut app CatchallRouteOrderApp) any(mut ctx CatchallRouteOrderContext, path string) veb.Result {
	ctx.res.set_status(.not_found)
	return ctx.text('fallback ${path}')
}

@['/api/user/:id']
fn (mut app CatchallRouteOrderApp) get_user(mut ctx CatchallRouteOrderContext, id string) veb.Result {
	return ctx.text('user ${id}')
}

fn test_variadic_routes_do_not_override_more_specific_handlers() {
	mut app := &CatchallRouteOrderApp{}
	spawn veb.run_at[CatchallRouteOrderApp, CatchallRouteOrderContext](mut app,
		port:                 catchall_route_order_port
		show_startup_message: false
		timeout_in_seconds:   1
		family:               .ip
	)
	_ := <-app.started
	app.server.wait_till_running(max_retries: 1000, retry_period_ms: 10)!
	defer {
		app.server.shutdown(timeout: 5 * time.second) or {}
	}

	user := http.get('http://127.0.0.1:${catchall_route_order_port}/api/user/42')!
	assert user.status() == .ok
	assert user.body == 'user 42'

	missing := http.get('http://127.0.0.1:${catchall_route_order_port}/missing/path')!
	assert missing.status() == .not_found
	assert missing.body == 'fallback /missing/path'
}
