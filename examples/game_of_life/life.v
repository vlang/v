import term
import rand
import time

const (
	cell     = '█'
	nothing  = ' '
	switches = {
		cell:    nothing
		nothing: cell
	}
	transformers = [nothing, cell]
)

struct Game {
mut:
	grid [][]string
}

fn (g Game) get_surrounding_alive_count(x int, y int) int {
	mut count := 0
	for i := x - 1; i <= x + 1; i++ {
		for j := y - 1; j <= y + 1; j++ {
			if (i != x || j != y) && i >= 0 && j >= 0 && i < g.grid.len && j < g.grid[x].len {
				if g.grid[i][j] == cell {
					count++
				}
			}
		}
	}
	return count
}

fn (mut g Game) evolve() {
	mut temp_grid := [][]string{}
	for x in 0 .. g.grid.len {
		temp_grid << []string{}
		for y in 0 .. g.grid[x].len {
			count := g.get_surrounding_alive_count(x, y)
			if count == 3 || ((g.grid[x][y] == cell) && count == 2) {
				temp_grid[x] << cell
			} else {
				temp_grid[x] << nothing
			}
		}
	}

	g.grid = temp_grid
}

fn (mut g Game) display() {
	for y in 0 .. g.grid[0].len {
		mut line := ''
		for x in 0 .. g.grid.len {
			line += g.grid[x][y]
		}
		println(line)
	}
}

fn new_game() Game {
	w, h := term.get_terminal_size()
	mut grid := [][]string{}
	for x in 0 .. w {
		grid << []string{}
		for _ in 0 .. h {
			is_live := rand.f64() > 0.82
			icon := transformers[int(is_live)]
			grid[x] << icon
		}
	}
	return Game{grid}
}

fn main() {
	mut g := new_game()

	g.display()
	for {
		g.evolve()
		term.erase_clear()
		g.display()
		time.sleep(100 * time.millisecond)
	}
}
