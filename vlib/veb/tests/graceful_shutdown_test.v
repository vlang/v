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
	server_ready chan &veb.Server = chan &veb.Server{cap: 1}
	slow_started chan bool        = chan bool{cap: 1}
	slow_release chan bool        = chan bool{cap: 1}
}

// init_server publishes the lifecycle handle before the server starts listening.
pub fn (mut app GracefulShutdownApp) init_server(server &veb.Server) {
	app.server_ready <- server
}

// slow holds the response until the test releases the in-flight request.
@['/slow']
pub fn (mut app GracefulShutdownApp) slow(mut ctx GracefulShutdownContext) veb.Result {
	app.slow_started <- true
	select {
		_ := <-app.slow_release {
		}
		graceful_shutdown_wait_timeout {
			panic('slow request was never released')
		}
	}
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
	server.shutdown(timeout: 5 * time.second, retry_period_ms: 1) or {
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

fn wait_for_graceful_server(ready chan &veb.Server) !&veb.Server {
	select {
		server := <-ready {
			server.wait_till_running(max_retries: 2000, retry_period_ms: 1)!
			return server
		}
		graceful_shutdown_wait_timeout {
		}
	}
	return error('veb server was not initialized')
}

fn wait_for_listener_to_close(port int) ! {
	addr := '${graceful_shutdown_host}:${port}'
	timer := time.new_stopwatch()
	for timer.elapsed() < graceful_shutdown_wait_timeout {
		mut conn := net.dial_tcp(addr) or { return }
		conn.close() or {}
		time.sleep(time.millisecond)
	}
	return error('server did not stop accepting connections in time')
}

@[unsafe]
fn test_veb_graceful_shutdown_waits_for_in_flight_requests() {
	mut listener := net.listen_tcp(.ip, '${graceful_shutdown_host}:0') or { panic(err) }
	port := listener.addr()!.port()!
	listener.close() or {}

	mut app := &GracefulShutdownApp{}
	defer { app.slow_release.close() }
	server_thread := spawn run_graceful_shutdown_app(mut app, int(port))

	server := wait_for_graceful_server(app.server_ready)!

	slow_responses := chan http.Response{cap: 1}
	spawn send_slow_request(int(port), slow_responses)
	wait_for_slow_request(app.slow_started) or {
		assert false, err.msg()
		return
	}

	shutdown_done := chan string{cap: 1}
	spawn shutdown_graceful_server(server, shutdown_done)
	wait_for_listener_to_close(int(port))!
	select {
		shutdown_error := <-shutdown_done {
			assert false, 'shutdown returned while the request was still in flight: ${shutdown_error}'
			return
		}
		else {
		}
	}
	app.slow_release <- true

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
