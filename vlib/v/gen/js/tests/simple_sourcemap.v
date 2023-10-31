module main

fn main() {
	e := JS.Error{}
	s := e.stack
	node_version := js_node_process().version
	node_main := get_node_main_version(node_version)
	if node_main >= 12 {
		if s.contains('simple_sourcemap.v:') {
			panic('node found no source map!')
		} else {
			println('source map is working')
		}
	} else {
		println('skiping test! node version >=12.12.0 required. Current Version is ${node_version}')
	}
}

fn get_node_main_version(str string) int {
	a := str.slice(1, int(str.len))
	b := a.split('.')
	return b[0].int()
}
