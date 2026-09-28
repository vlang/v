// vtest build: linux
module fasthttp

import sync.stdatomic
import time

@[heap]
struct StartupProbe {
mut:
	server          &Server                   = unsafe { nil }
	started_workers &stdatomic.AtomicVal[int] = stdatomic.new_atomic(0)
	saw_stopped     &stdatomic.AtomicVal[int] = stdatomic.new_atomic(0)
}

fn startup_probe_handler(_ HttpRequest) !HttpResponse {
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n'.bytes()
	}
}

fn run_startup_probe_server(mut server Server) {
	server.run() or { panic(err) }
}

// A worker serves requests as soon as it runs, and shutdown() returns at once for
// a server marked stopped. A worker that starts before run() clears that mark lets
// a shutdown requested in that window report success without stopping anything.
fn test_workers_start_on_a_server_marked_live() ! {
	mut probe := &StartupProbe{}
	mut server := new_server(ServerConfig{
		family:     .ip
		host:       '127.0.0.1'
		port:       0
		handler:    startup_probe_handler
		make_state: fn [probe] () voidptr {
			mut started := probe.started_workers
			started.add(1)
			if probe.server.is_stopped() {
				mut saw_stopped := probe.saw_stopped
				saw_stopped.add(1)
			}
			return unsafe { nil }
		}
	})!
	probe.server = server
	server_thread := spawn run_startup_probe_server(mut server)
	server.wait_till_running_impl(max_retries: 1000, retry_period_ms: 10)!
	server.shutdown_impl(timeout: 10 * time.second)!
	server_thread.wait()
	mut started := probe.started_workers
	mut saw_stopped := probe.saw_stopped
	assert started.load() == max_thread_pool_size
	assert saw_stopped.load() == 0
}
