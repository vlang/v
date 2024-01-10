module dotgraph

pub fn start_digraph() {
	println('digraph G {')
	C.atexit(fn () {
		println('}')
	})
}
