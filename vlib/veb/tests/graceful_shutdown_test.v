// vtest build: !windows && !tinyc // fasthttp.Server.run is not implemented on windows; the test hangs intermittently when built with tcc on linux CI
import net
import net.http
import time
import veb

const graceful_shutdown_host = '127.0.0.1'
const graceful_shutdown_wait_timeout = 10 * time.second

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

fn shutdown_graceful_server(server &veb.Server, done chan string) {
	server.shutdown(timeout: 5 * time.second) or {
		done <- err.msg()
		return
	}
	done <- ''
}

fn wait_for_slow_request(started chan bool) ! {
	select {
		_ := <-started {
			return
		}
		graceful_shutdown_wait_timeout {
			return error('slow request did not start')
		}
	}
}

fn wait_for_slow_response(responses chan http.Response) !http.Response {
	select {
		response := <-responses {
			return response
		}
		graceful_shutdown_wait_timeout {
			return error('slow request did not finish')
		}
	}
	return error('slow request did not finish')
}

fn wait_for_shutdown(done chan string) ! {
	select {
		shutdown_error := <-done {
			if shutdown_error != '' {
				return error(shutdown_error)
			}
			return
		}
		graceful_shutdown_wait_timeout {
			return error('server shutdown did not finish')
		}
	}
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
	wait_for_slow_request(app.slow_started) or {
		assert false, err.msg()
		return
	}

	mut server := app.server
	if server == unsafe { nil } {
		assert false, 'veb server was not initialized'
		return
	}
	shutdown_done := chan string{cap: 1}
	spawn shutdown_graceful_server(server, shutdown_done)

	slow_response := wait_for_slow_response(slow_responses) or {
		assert false, err.msg()
		return
	}
	assert slow_response.status() == .ok
	assert slow_response.body == 'slow done'

	wait_for_shutdown(shutdown_done) or {
		assert false, err.msg()
		return
	}
	server_thread.wait()

	http.get('http://${graceful_shutdown_host}:${port}/') or {
		assert true
		return
	}
	assert false, 'server still accepts new requests after shutdown'
}
