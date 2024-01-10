import math

struct Cell {
	p_i int
	p_j int
	f   f64 = math.max_f64
	g   f64 = math.max_f64
	h   f64 = math.max_f64
}

fn test_array_init_element_size_equal_array_size() {
	mut cells := [][]Cell{len: 1, init: []Cell{len: 1}}
	println(cells)
	assert '${cells[0][0].p_i}' == '0'
	assert '${cells[0][0].p_j}' == '0'
	assert '${cells[0][0].f}' == '1.7976931348623157e+308'
	assert '${cells[0][0].g}' == '1.7976931348623157e+308'
	assert '${cells[0][0].h}' == '1.7976931348623157e+308'
}
