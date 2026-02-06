module dotgraph

pub fn start_digraph() {
	println('digraph G {')
	#JS.process.on('exit', fn () { println('}') })
}
