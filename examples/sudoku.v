fn main() {
	mut grid := [
		[0, 3, 0, 0, 7, 0, 0, 0, 0],
		[0, 0, 0, 1, 3, 5, 0, 0, 0],
		[0, 0, 1, 0, 0, 0, 0, 5, 0],
		[1, 0, 0, 0, 6, 0, 0, 0, 3],
		[4, 0, 0, 8, 0, 3, 0, 0, 1],
		[7, 0, 0, 0, 2, 0, 0, 0, 6],
		[0, 0, 0, 0, 0, 0, 2, 1, 0],
		[0, 0, 0, 4, 1, 2, 0, 0, 5],
		[0, 0, 0, 0, 0, 0, 0, 7, 4],
	]
	print_grid('Sudoku Puzzle:', grid)
	println('Solving...')
	if solve_sudoku(mut grid) {
		print_grid('Solution:', grid)
	} else {
		println('No solution exists.')
		exit(1)
	}
}

// is_valid checks if placing `num` at grid[row][col] is valid
fn is_valid(grid [][]int, row int, col int, num int) bool {
	// check the row, if the number has been placed already:
	for x := 0; x < 9; x++ {
		if grid[row][x] == num {
			return false
		}
	}
	// check column
	for x := 0; x < 9; x++ {
		if grid[x][col] == num {
			return false
		}
	}
	// check 3x3 subgrid
	start_row := row - row % 3
	start_col := col - col % 3
	for i := 0; i < 3; i++ {
		for j := 0; j < 3; j++ {
			if grid[i + start_row][j + start_col] == num {
				return false
			}
		}
	}
	return true
}

// find_empty finds an empty cell (0) in the grid:
fn find_empty(grid [][]int) ?(int, int) {
	for i := 0; i < 9; i++ {
		for j := 0; j < 9; j++ {
			if grid[i][j] == 0 {
				return i, j
			}
		}
	}
	return none
}

// solve_sudoku solves the Sudoku puzzle using backtracking
fn solve_sudoku(mut grid [][]int) bool {
	// If there is no empty cell, the puzzle is solved:
	row, col := find_empty(grid) or { return true }
	// Try placing all the digits in turn in the empty cell:
	for num := 1; num <= 9; num++ {
		if is_valid(grid, row, col, num) {
			grid[row][col] = num
			// Recursively try to solve the rest
			if solve_sudoku(mut grid) {
				return true
			}
			// We could not find a solution using this number,
			// so backtrack and try another number instead:
			grid[row][col] = 0
		}
	}
	return false
}

// print_grid prints a labeled Sudoku grid
fn print_grid(label string, grid [][]int) {
	println(label)
	for i := 0; i < 9; i++ {
		if i % 3 == 0 && i != 0 {
			println('- - - - - - - - - - - -')
		}
		for j := 0; j < 9; j++ {
			if j % 3 == 0 && j != 0 {
				print(' | ')
			}
			print('${grid[i][j]} ')
		}
		println('')
	}
}
