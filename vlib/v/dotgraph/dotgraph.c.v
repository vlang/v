module dotgraph

pub fn start_digraph() {
	println('digraph G {')
	at_exit(fn () {
		println('}')
	}) or {}
}
