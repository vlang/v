@[has_globals]
module main

@[export: 'TIC']
__global tic = fn (a int) int {
	return a + 1
}

fn main() {
	assert int(JS.eval(c'(globalThis.TIC === globalThis.tic) ? 1 : 0')) == 1
	assert int(JS.eval(c'typeof globalThis.TIC === "function" ? globalThis.TIC(4) : -1')) == 5
	assert tic(2) == 3
}
