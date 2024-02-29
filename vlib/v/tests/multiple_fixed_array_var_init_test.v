struct Cell {
	value  int
	fvalue f32
mut:
	mut_value  int = 10
	mut_fvalue f32
}

struct Grid {
	cells [2][2][2]Cell
}

fn test_multiple_fixed_array_var_init() {
	mut tile_field := [2][2][2]Cell{}

	b := Grid{
		cells: tile_field
	}

	println('The original:')
	println(tile_field)
	println('==========')
	println('The struct:')
	println(b.cells)
	assert b.cells[0][0][0].value == 0
	assert b.cells[0][0][0].mut_value == 10
	assert b.cells[0][0][1].value == 0
	assert b.cells[0][0][1].mut_value == 10
	assert b.cells[0][1][0].value == 0
	assert b.cells[0][1][0].mut_value == 10
	assert b.cells[0][1][1].value == 0
	assert b.cells[0][1][1].mut_value == 10
	assert b.cells[1][0][0].value == 0
	assert b.cells[1][0][0].mut_value == 10
	assert b.cells[1][0][1].value == 0
	assert b.cells[1][0][1].mut_value == 10
	assert b.cells[1][1][0].value == 0
	assert b.cells[1][1][0].mut_value == 10
	assert b.cells[1][1][1].value == 0
	assert b.cells[1][1][1].mut_value == 10
}
