module main

interface Backend {
	foo()
}

struct MetalBackend {}

fn (m MetalBackend) foo() {}

fn (m MetalBackend) bar() {
	println('bar')
}

fn make_backend() Backend {
	return MetalBackend{}
}

fn main() {
	mut backend := make_backend()
	if mut backend is MetalBackend {
		backend.bar()
	}
}
