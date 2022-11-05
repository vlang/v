module main

fn test_go_call_fn_using_map_value() {
	sum := fn (x int, y int) int {
		return x + y
	}

	mut fns := map[string]fn (int, int) int{}
	fns['sum'] = sum

	g := spawn fns['sum'](2, 3)
	x := g.wait()

	println('$x')
	assert x == 5
}
