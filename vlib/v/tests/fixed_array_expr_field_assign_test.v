const fixed_array_expr_hi = [u8(1), 2, 3, 4]!
const fixed_array_expr_lo = [u8(5), 6, 7, 8]!

struct FixedArrayExprMesh {
mut:
	highlight [4]u8
}

fn fixed_array_expr_set_if(mut mesh FixedArrayExprMesh, highlighted bool) {
	mesh.highlight = if highlighted { fixed_array_expr_hi } else { fixed_array_expr_lo }
}

fn fixed_array_expr_set_match(mut mesh FixedArrayExprMesh, highlighted bool) {
	mesh.highlight = match highlighted {
		true { fixed_array_expr_hi }
		false { fixed_array_expr_lo }
	}
}

fn test_fixed_array_field_assign_from_if_expr() {
	mut mesh := FixedArrayExprMesh{}

	fixed_array_expr_set_if(mut mesh, true)
	assert mesh.highlight == fixed_array_expr_hi

	fixed_array_expr_set_if(mut mesh, false)
	assert mesh.highlight == fixed_array_expr_lo
}

fn test_fixed_array_field_assign_from_match_expr() {
	mut mesh := FixedArrayExprMesh{}

	fixed_array_expr_set_match(mut mesh, true)
	assert mesh.highlight == fixed_array_expr_hi

	fixed_array_expr_set_match(mut mesh, false)
	assert mesh.highlight == fixed_array_expr_lo
}

struct FixedArrayExprBoard {
mut:
	cells [4][4]int
}

fn fixed_array_expr_board_of(values [][]int) [4][4]int {
	mut board := [4][4]int{}
	for row in 0 .. 4 {
		for column in 0 .. 4 {
			board[row][column] = values[row][column]
		}
	}
	return board
}

fn test_fixed_array_field_assign_from_call_with_array_init_argument() {
	mut board := FixedArrayExprBoard{}
	board.cells = fixed_array_expr_board_of([
		[0, 0, 0, 2],
		[0, 0, 0, 0],
		[0, 2, 0, 0],
		[0, 0, 0, 0],
	])
	assert board.cells[0][3] == 2
	assert board.cells[2][1] == 2
}
