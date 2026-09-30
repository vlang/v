@[has_globals]
module main

// Translated from Go's src/runtime/race/testdata/io_test.go, see ../README.md.
import net
import net.http
import os
import sync
import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_no_race_io_file', test_no_race_io_file)
	run('test_no_race_io_http', test_no_race_io_http)
	eprintln('=== DONE')
}

// run starts a test like `go test -v` does, so the race reports that follow belong to it.
// Go's harness runs the tests with GOMAXPROCS=1, where the goroutines of a test run as soon
// as it blocks; V threads run in parallel, so give those that outlive the test, and may
// still report a race, a moment to do so before the next test starts.
fn run(name string, test fn ()) {
	eprintln('=== RUN   ${name}')
	test()
	time.sleep(20 * time.millisecond)
}

fn test_no_race_io_file() {
	mut x := &Cell[int]{}
	// Go's t.TempDir(): a new directory, removed at the end of the test.
	path := os.join_path(os.vtmp_dir(), 'go_race_io_file_${os.getpid()}')
	os.mkdir_all(path) or { panic(err) }
	defer {
		os.rmdir_all(path) or {}
	}
	fname := os.join_path(path, 'data')
	spawn fn [mut x, fname] () {
		x.v = 42
		mut f := os.create(fname) or { panic(err) }
		f.write('done'.bytes()) or {}
		f.close()
	}()
	for {
		f := os.open(fname) or {
			time.sleep(time.millisecond)
			continue
		}
		mut buf := []u8{len: 100}
		count := f.read(mut buf) or { 0 }
		if count == 0 {
			time.sleep(time.millisecond)
			continue
		}
		break
	}
	_ = x.v
}

// ServeMux stands for Go's http.DefaultServeMux, in which http.HandleFunc registers the
// handler: V's http.Server takes its handler directly.
struct ServeMux {
mut:
	handler fn (mut w http.Response) = unsafe { nil }
}

fn (mut m ServeMux) handle(_ http.Request) http.Response {
	mut w := http.Response{}
	m.handler(mut w)
	return w
}

__global reg_handler = sync.new_once()
__global handler_data int
__global default_serve_mux = &ServeMux{}

// serve is Go's http.Serve(ln, handler): it serves the connections accepted on ln.
fn serve(ln &net.TcpListener, handler http.Handler) {
	mut s := http.Server{
		listener:             *ln
		handler:              handler
		show_startup_message: false
	}
	s.listen_and_serve()
}

fn test_no_race_io_http() {
	reg_handler.do(fn () {
		default_serve_mux.handler = fn (mut w http.Response) {
			handler_data++
			w.body = 'test' // Go's fmt.Fprintf(w, "test")
			handler_data++
		}
	})
	mut ln := net.listen_tcp(.ip, '127.0.0.1:0') or { panic('net.Listen: ${err}') }
	defer {
		ln.close() or {}
	}
	addr := ln.addr() or { panic(err) }
	spawn serve(ln, default_serve_mux)
	handler_data++
	http.get('http://${addr}') or { panic('http.Get: ${err}') }
	handler_data++
	http.get('http://${addr}') or { panic('http.Get: ${err}') }
	handler_data++
}
