import net
import net.http
import time
import veb

fn test_stub() {
	$if new_veb ? {
		return
	}
}

$if !new_veb ? {
	const parallel_event_loop_host = '127.0.0.1'

	struct ParallelEventLoopContext {
		veb.Context
	}

	struct ParallelEventLoopApp {}

	fn (app &ParallelEventLoopApp) index(mut ctx ParallelEventLoopContext) veb.Result {
		return ctx.text('ready')
	}

	@['/slow']
	fn (app &ParallelEventLoopApp) slow(mut ctx ParallelEventLoopContext) veb.Result {
		time.sleep(500 * time.millisecond)
		return ctx.text('slow done')
	}

	fn run_event_loop_app(mut app ParallelEventLoopApp, port int, nr_workers int) {
		veb.run_at[ParallelEventLoopApp, ParallelEventLoopContext](mut app,
			host:                 parallel_event_loop_host
			port:                 port
			family:               .ip
			nr_workers:           nr_workers
			show_startup_message: false
			timeout_in_seconds:   3
		) or { panic(err) }
	}

	fn wait_for_parallel_event_loop_server(port int) ! {
		url := 'http://${parallel_event_loop_host}:${port}/'
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

	fn fetch_parallel_event_loop_response(port int, responses chan http.Response) {
		response := http.get('http://${parallel_event_loop_host}:${port}/slow') or { panic(err) }
		responses <- response
	}

	fn next_parallel_event_loop_port() int {
		mut listener := net.listen_tcp(.ip, '${parallel_event_loop_host}:0') or { panic(err) }
		addr := listener.addr() or { panic(err) }
		port := addr.port() or { panic(err) }
		listener.close() or {}
		return int(port)
	}

	fn test_single_picoev_worker_still_serves_requests() {
		$if !(linux || termux) {
			return
		}
		port := next_parallel_event_loop_port()
		mut app := &ParallelEventLoopApp{}
		spawn run_event_loop_app(mut app, port, 1)

		wait_for_parallel_event_loop_server(port) or {
			assert false, err.msg()
			return
		}

		response := http.get('http://${parallel_event_loop_host}:${port}/slow') or {
			assert false, err.msg()
			return
		}
		assert response.status() == .ok
		assert response.body == 'slow done'
	}

	fn test_parallel_picoev_workers_handle_requests_concurrently() {
		$if !(linux || termux) {
			return
		}
		port := next_parallel_event_loop_port()
		mut app := &ParallelEventLoopApp{}
		spawn run_event_loop_app(mut app, port, 2)

		wait_for_parallel_event_loop_server(port) or {
			assert false, err.msg()
			return
		}

		responses := chan http.Response{cap: 2}
		watch := time.new_stopwatch(auto_start: true)
		spawn fetch_parallel_event_loop_response(port, responses)
		spawn fetch_parallel_event_loop_response(port, responses)

		first := <-responses or {
			assert false, err.msg()
			return
		}
		second := <-responses or {
			assert false, err.msg()
			return
		}
		assert first.status() == .ok
		assert second.status() == .ok
		assert first.body == 'slow done'
		assert second.body == 'slow done'
		assert watch.elapsed() < 900 * time.millisecond
	}

	fn test_parallel_picoev_workers_reject_invalid_nr_workers() {
		mut app := &ParallelEventLoopApp{}
		veb.run_at[ParallelEventLoopApp, ParallelEventLoopContext](mut app,
			host:                 parallel_event_loop_host
			port:                 1
			family:               .ip
			nr_workers:           0
			show_startup_message: false
		) or {
			assert err.msg().contains('invalid nr_workers')
			return
		}
		assert false, 'run_at should reject nr_workers <= 0'
	}
}
