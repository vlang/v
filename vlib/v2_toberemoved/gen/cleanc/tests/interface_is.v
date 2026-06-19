module main

interface Backend {}

struct MetalBackend {}

struct NullBackend {}

fn make_backend() Backend {
	return MetalBackend{}
}

fn main() {
	mut backend := make_backend()
	if mut backend is MetalBackend {
		println('metal')
	} else {
		println('not-metal')
	}
	if mut backend is NullBackend {
		println('null')
	} else {
		println('not-null')
	}
}
