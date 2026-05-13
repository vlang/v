import context
import net.http
import time
import x.async as xasync

fn test_net_http_pool_handles_synthetic_requests_with_backpressure() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	started := chan bool{cap: 1}
	release := chan bool{cap: 2}
	responses := chan int{cap: 2}
	first_req := http.new_request(.get, '/first', '')
	second_req := http.new_request(.get, '/second', '')
	third_req := http.new_request(.get, '/third', '')

	pool.try_submit(fn [first_req, started, release, responses] (mut ctx context.Context) ! {
		_ = ctx
		started <- true
		_ := <-release
		resp := synthetic_http_response(first_req)!
		responses <- resp.status_code
	})!
	wait_for_http_signal(started, 'first synthetic HTTP job did not start')

	pool.try_submit(fn [second_req, release, responses] (mut ctx context.Context) ! {
		_ = ctx
		_ := <-release
		resp := synthetic_http_response(second_req)!
		responses <- resp.status_code
	})!
	pool.try_submit(fn [third_req] (mut ctx context.Context) ! {
		_ = ctx
		_ = third_req
	}) or {
		assert err.msg() == 'async: pool queue is full'
		release <- true
		release <- true
		pool.close()!
		assert read_http_status(responses) == 200
		assert read_http_status(responses) == 200
		return
	}
	assert false
}

fn synthetic_http_response(req http.Request) !http.Response {
	if req.url == '' {
		return error('empty synthetic HTTP URL')
	}
	return http.new_response(status: .ok, body: req.url)
}

fn wait_for_http_signal(signal chan bool, message string) {
	select {
		ok := <-signal {
			assert ok
		}
		1 * time.second {
			assert false, message
		}
	}
}

fn read_http_status(responses chan int) int {
	select {
		status := <-responses {
			return status
		}
		1 * time.second {
			assert false, 'synthetic HTTP response was not produced'
		}
	}
	return 0
}
