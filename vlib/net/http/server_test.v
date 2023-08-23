import net
import net.http
import time

fn test_server_stop() {
	mut server := &http.Server{
		accept_timeout: 1 * time.second
	}
	t := spawn server.listen_and_serve()
	time.sleep(250 * time.millisecond)
	mut watch := time.new_stopwatch()
	server.stop()
	assert server.status() == .stopped
	assert watch.elapsed() < 100 * time.millisecond
	t.wait()
	assert watch.elapsed() < 999 * time.millisecond
}

fn test_server_close() {
	mut server := &http.Server{
		accept_timeout: 1 * time.second
		handler: MyHttpHandler{}
	}
	t := spawn server.listen_and_serve()
	time.sleep(250 * time.millisecond)
	mut watch := time.new_stopwatch()
	server.close()
	assert server.status() == .closed
	assert watch.elapsed() < 100 * time.millisecond
	t.wait()
	assert watch.elapsed() < 999 * time.millisecond
}

fn test_server_custom_listener() {
	listener := net.listen_tcp(.ip6, ':8081')!
	mut server := &http.Server{
		accept_timeout: 1 * time.second
		listener: listener
	}
	t := spawn server.listen_and_serve()
	time.sleep(250 * time.millisecond)
	mut watch := time.new_stopwatch()
	server.close()
	assert server.status() == .closed
	assert watch.elapsed() < 100 * time.millisecond
	t.wait()
	assert watch.elapsed() < 999 * time.millisecond
}

struct MyHttpHandler {
mut:
	counter    int
	oks        int
	not_founds int
	redirects  int
}

fn (mut handler MyHttpHandler) handle(req http.Request) http.Response {
	handler.counter++
	// eprintln('$time.now() | counter: $handler.counter | $req.method $req.url\n$req.header\n$req.data - 200 OK\n')
	mut r := http.Response{
		body: req.data + ', ${req.url}'
		header: req.header
	}
	match req.url.all_before('?') {
		'/endpoint', '/another/endpoint' {
			r.set_status(.ok)
			handler.oks++
		}
		'/redirect_to_big' {
			r.header = http.new_header(key: .location, value: '/big')
			r.status_msg = 'Moved permanently'
			r.status_code = 301
			handler.redirects++
		}
		'/big' {
			r.body = 'xyz def '.repeat(10_000)
			r.set_status(.ok)
			handler.oks++
		}
		else {
			r.set_status(.not_found)
			handler.not_founds++
		}
	}
	r.set_version(req.version)
	return r
}

const cport = 8198

fn test_server_custom_handler() {
	mut handler := MyHttpHandler{}
	mut server := &http.Server{
		accept_timeout: 1 * time.second
		handler: handler
		port: cport
	}
	t := spawn server.listen_and_serve()
	for server.status() != .running {
		time.sleep(10 * time.millisecond)
	}
	x := http.fetch(url: 'http://localhost:${cport}/endpoint?abc=xyz', data: 'my data')!
	assert x.body == 'my data, /endpoint?abc=xyz'
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.http_version == '1.1'
	y := http.fetch(url: 'http://localhost:${cport}/another/endpoint', data: 'abcde')!
	assert y.body == 'abcde, /another/endpoint'
	assert y.status_code == 200
	assert x.status_msg == 'OK'
	assert y.status() == .ok
	assert y.http_version == '1.1'
	//
	http.fetch(url: 'http://localhost:${cport}/something/else')!
	//
	big_url := 'http://localhost:${cport}/redirect_to_big'
	mut progress_calls := &ProgressCalls{}
	z := http.fetch(
		url: big_url
		on_redirect: fn [mut progress_calls] (req &http.Request, nredirects int, new_url string) {
			eprintln('>>>>>>>> on_redirect, req.url: ${req.url} | new_url: ${new_url} | nredirects: ${nredirects}')
			progress_calls.redirected_to << new_url
		}
		on_progress: fn [mut progress_calls] (req &http.Request, chunk []u8, read_so_far u64) {
			eprintln('>>>>>>>> on_progress, req.url: ${req.url} | got chunk.len: ${chunk.len:5}, read_so_far: ${read_so_far:8}, chunk: ${chunk#[0..30].bytestr()}')
			progress_calls.chunks << chunk
			progress_calls.reads << read_so_far
		}
		on_finish: fn [mut progress_calls] (req &http.Request) {
			eprintln('>>>>>>>> on_finish, req.url: ${req.url}')
			progress_calls.finished_was_called = true
		}
	)!
	assert z.status_code == 200
	assert z.body.starts_with('xyz')
	assert z.body.len > 10000
	assert progress_calls.finished_was_called
	assert progress_calls.chunks.len > 1
	assert progress_calls.reads.len > 1
	assert progress_calls.chunks[0].bytestr().starts_with('HTTP/1.1 301 Moved permanently')
	assert progress_calls.chunks[1].bytestr().starts_with('HTTP/1.1 200 OK')
	assert progress_calls.chunks.last().bytestr().contains('xyz def')
	assert progress_calls.redirected_to == ['http://localhost:8198/big']
	//
	server.stop()
	t.wait()
	//
	assert handler.counter == 5
	assert handler.oks == 3
	assert handler.not_founds == 1
	assert handler.redirects == 1
}

struct ProgressCalls {
mut:
	chunks              [][]u8
	reads               []u64
	finished_was_called bool
	redirected_to       []string
}
