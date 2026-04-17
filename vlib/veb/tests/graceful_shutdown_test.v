import net
import net.http
import time
import veb

const graceful_shutdown_host = '127.0.0.1'

struct GracefulShutdownContext {
	veb.Context
}

struct GracefulShutdownApp {
mut:
	server       &veb.Server = unsafe { nil }
	slow_started chan bool   = chan bool{cap: 1}
}

pub fn (mut app GracefulShutdownApp) init_server(server &veb.Server) {
	app.server = server
}

pub fn (mut app GracefulShutdownApp) index(mut ctx GracefulShutdownContext) veb.Result {
	return ctx.text('ready')
}

@['/slow']
pub fn (mut app GracefulShutdownApp) slow(mut ctx GracefulShutdownContext) veb.Result {
	app.slow_started <- true
	time.sleep(300 * time.millisecond)
	return ctx.text('slow done')
}

pub fn (mut app GracefulShutdownApp) shutdown(mut ctx GracefulShutdownContext) veb.Result {
	spawn app.shutdown_now()
	return ctx.text('shutting down')
}

fn (app &GracefulShutdownApp) shutdown_now() {
	mut server := app.server
	if server == unsafe { nil } {
		panic('veb server was not initialized')
	}
	server.shutdown() or { panic(err) }
}

fn run_graceful_shutdown_app(mut app GracefulShutdownApp, port int) {
	veb.run_at[GracefulShutdownApp, GracefulShutdownContext](mut app,
		host:                 graceful_shutdown_host
		port:                 port
		family:               .ip
		show_startup_message: false
	) or { panic(err) }
}

fn send_slow_request(port int, responses chan http.Response) {
	response := http.get('http://${graceful_shutdown_host}:${port}/slow') or { panic(err) }
	responses <- response
}

fn wait_for_server(port int) ! {
	url := 'http://${graceful_shutdown_host}:${port}/'
	for _ in 0 .. 100 {
		response := http.get(url) or {
			time.sleep(20 * time.millisecond)
			continue
		}
		if response.status() == .ok && response.body == 'ready' {
			return
		}
		time.sleep(20 * time.millisecond)
	}
	return error('server did not start listening in time')
}

fn test_veb_graceful_shutdown_waits_for_in_flight_requests() {
	mut listener := net.listen_tcp(.ip, '${graceful_shutdown_host}:0') or { panic(err) }
	port := listener.addr()!.port()!
	listener.close() or {}

	mut app := &GracefulShutdownApp{}
	server_thread := spawn run_graceful_shutdown_app(mut app, int(port))

	wait_for_server(int(port)) or {
		assert false, err.msg()
		return
	}

	slow_responses := chan http.Response{cap: 1}
	spawn send_slow_request(int(port), slow_responses)
	_ := <-app.slow_started or {
		assert false, 'slow request did not start'
		return
	}

	shutdown_response := http.get('http://${graceful_shutdown_host}:${port}/shutdown') or {
		assert false, err.msg()
		return
	}
	assert shutdown_response.status() == .ok
	assert shutdown_response.body == 'shutting down'

	slow_response := <-slow_responses or {
		assert false, err.msg()
		return
	}
	assert slow_response.status() == .ok
	assert slow_response.body == 'slow done'

	server_thread.wait()

	http.get('http://${graceful_shutdown_host}:${port}/') or {
		assert true
		return
	}
	assert false, 'server still accepts new requests after shutdown'
}
