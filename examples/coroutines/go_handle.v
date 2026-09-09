// vtest build: false // This should be built with: `v -use-coroutines go_handle.v`
// Note: the Photon wrapper is not yet trivial enough to build/install on the CI.
//
// Demonstrates storing the handle returned by a `go` expression and waiting on
// it. Because the Photon coroutine wrapper exposes no joinable handle, a `go`
// whose handle is used is lowered to the regular `spawn` (pthread) path, so the
// handle can be waited on just like `spawn`.
module main

struct App {
mut:
	worker thread
}

fn (a App) loop() {
	println('hello from the worker coroutine')
}

fn main() {
	mut a := App{}
	a.worker = go a.loop()
	a.worker.wait()
	println('done')
}
