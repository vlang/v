// vtest build: !windows // fasthttp.Server.run is not implemented on windows yet
import net.http
import time
import veb

const catchall_route_order_port = 13062

struct CatchallRouteOrderContext {
	veb.Context
}

struct CatchallRouteOrderApp {
mut:
	server_ready chan &veb.Server = chan &veb.Server{cap: 1}
}

// init_server publishes the lifecycle handle before the server starts listening.
pub fn (mut app CatchallRouteOrderApp) init_server(server &veb.Server) {
	app.server_ready <- server
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

fn wait_for_catchall_server(ready chan &veb.Server) !&veb.Server {
	select {
		server := <-ready {
			server.wait_till_running(max_retries: 10_000, retry_period_ms: 1)!
			return server
		}
		10 * time.second {
		}
	}
	return error('veb server was not initialized')
}

fn test_variadic_routes_do_not_override_more_specific_handlers() {
	mut app := &CatchallRouteOrderApp{}
	server_thread := spawn veb.run_at[CatchallRouteOrderApp, CatchallRouteOrderContext](mut app,
		port:                 catchall_route_order_port
		show_startup_message: false
		timeout_in_seconds:   1
		family:               .ip
	)
	server := wait_for_catchall_server(app.server_ready)!
	defer {
		server.shutdown(timeout: 5 * time.second, retry_period_ms: 1) or { panic(err) }
		server_thread.wait() or { panic(err) }
	}

	user := http.get('http://127.0.0.1:${catchall_route_order_port}/api/user/42')!
	assert user.status() == .ok
	assert user.body == 'user 42'

	missing := http.get('http://127.0.0.1:${catchall_route_order_port}/missing/path')!
	assert missing.status() == .not_found
	assert missing.body == 'fallback /missing/path'
}
