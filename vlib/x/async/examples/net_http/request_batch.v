import context
import net.http
import x.async as xasync

fn synthetic_http_handler(req http.Request) !http.Response {
	path := if req.url.contains('?') { req.url.all_before('?') } else { req.url }
	match path {
		'/health' {
			return http.new_response(status: .ok, body: 'healthy')
		}
		'/ready' {
			return http.new_response(status: .ok, body: 'ready')
		}
		else {
			return http.new_response(status: .not_found, body: 'missing')
		}
	}
}

fn main() {
	requests := [
		http.new_request(.get, '/health', ''),
		http.new_request(.get, '/ready', ''),
		http.new_request(.get, '/missing', ''),
	]
	responses := chan string{cap: requests.len}
	mut pool := xasync.new_pool(workers: 2, queue_size: requests.len)!

	for req in requests {
		pool.try_submit(fn [req, responses] (mut ctx context.Context) ! {
			done := ctx.done()
			select {
				_ := <-done {
					return ctx.err()
				}
				else {}
			}
			resp := synthetic_http_handler(req)!
			responses <- '${req.url} -> ${resp.status_code} ${resp.body}'
		})!
	}

	pool.close()!
	for _ in 0 .. requests.len {
		println(<-responses)
	}
}
