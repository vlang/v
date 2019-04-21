import rand
import time
import gx
import gl
import gg
import glfw
import math

const (
	BLOCK_SIZE = 20 // pixels
	FIELD_HEIGHT = 20 // # of blocks
	FIELD_WIDTH = 10
	TETRO_SIZE = 4
	WIN_WIDTH = BLOCK_SIZE * FIELD_WIDTH
	WIN_HEIGHT = BLOCK_SIZE * FIELD_HEIGHT
	TIMER_PERIOD = 250 // ms
)

const (
	// Tetros and their 4 possible states are encoded in binaries
	B_TETROS = [
		// 0000 0
		// 0000 0
		// 0110 6
		// 0110 6
		[66, 66, 66, 66],
		// 0000 0
		// 0000 0
		// 0010 2
		// 0111 7
		[27, 131, 72, 232],
		// 0000 0
		// 0000 0
		// 0011 3
		// 0110 6
		[36, 231, 36, 231],
		// 0000 0
		// 0000 0
		// 0110 6
		// 0011 3
		[63, 132, 63, 132],
		// 0000 0
		// 0011 3
		// 0001 1
		// 0001 1
		[311, 17, 223, 74],
		// 0000 0
		// 0011 3
		// 0010 2
		// 0010 2
		[322, 71, 113, 47],
		// Special case since 15 can't be used
		// 1111
		[1111, 9, 1111, 9],
	]
	// Each tetro has its unique color
	COLORS = [
		gx.rgb(0, 0, 0),
		gx.rgb(253, 32, 47),
		gx.rgb(0, 110, 194),
		gx.rgb(34, 169, 16),
		gx.rgb(170, 0, 170),
		gx.rgb(0, 0, 170),
		gx.rgb(0, 170, 0),
		gx.rgb(170, 85, 0),
		gx.rgb(0, 170, 170),
	]
)

// TODO: type Tetro [TETRO_SIZE]struct{ x, y int }
struct Block {
	x int
	y int
}

struct Game {
	// Position of the current tetro
	pos_x        int
	pos_y        int
	// field[y][x] contains the color of the block with (x,y) coordinates
	// "-1" border is to avoid bounds checking.
	// -1 -1 -1 -1
	// -1  0  0 -1
	// -1  0  0 -1
	// -1 -1 -1 -1
	// TODO: field [][]int
	field       array_array_int
	// TODO: tetro Tetro
	tetro       []Block
	// TODO: tetros_cache []Tetro 
	tetros_cache []Block 
	// Index of the current tetro. Refers to its color.
	tetro_idx    int
	// Index of the rotation (0-3)
	rotation_idx int
	// gg context for drawing
	gg          *gg.GG
}

fn main() {
	mut game := &Game{}
	game.parse_tetros() 
	game.init_game()
	glfw.init()
	mut window := glfw.create_window(glfw.WinCfg {
		width: WIN_WIDTH
		height: WIN_HEIGHT
		title: 'V Tetris'
		ptr: game // glfw user pointer
	})
	window.make_context_current()
	window.onkeydown(key_down)
	gg.init()
	game.gg = gg.new_context(gg.Cfg {
		width: WIN_WIDTH
		height: WIN_HEIGHT
		use_ortho: true // This is needed for 2D drawing
	})
	go game.run() // Run the game loop in a new thread
	gl.clear() // For some reason this is necessary to avoid an intial flickering
	gl.clear_color(255, 255, 255, 255)
	for {
		gl.clear()
		gl.clear_color(255, 255, 255, 255)
		game.draw_scene()
		window.swap_buffers()
		glfw.wait_events()
	}
}

fn (g mut Game) init_game() {
	rand.seed()
	g.generate_tetro()
	g.field = []array_int // TODO: g.field = [][]int
	// Generate the field, fill it with 0's, add -1's on each edge
	for i := 0; i < FIELD_HEIGHT + 2; i++ {
		mut row := [0; FIELD_WIDTH + 2]
		row[0] = - 1
		row[FIELD_WIDTH + 1] = - 1
		g.field << row
	}
	mut first_row := g.field[0]
	mut last_row := g.field[FIELD_HEIGHT + 1]
	for j := 0; j < FIELD_WIDTH + 2; j++ {
		first_row[j] = - 1
		last_row[j] = - 1
	}
}

fn (g mut Game) parse_tetros() {
	for i, b_tetros in B_TETROS {
		for b_tetro in b_tetros { 
			for t in parse_binary_tetro(b_tetro) { 
				g.tetros_cache << t
			} 
		} 
	} 
} 

fn (g mut Game) run() {
	for {
		g.move_tetro()
		g.delete_completed_lines()
		glfw.post_empty_event() // force window redraw
		time.sleep_ms(TIMER_PERIOD)
	}
}

fn (g mut Game) move_tetro() {
	// Check each block in current tetro
	//for i := 0; i < TETRO_SIZE; i++ {
		//tetro := g.tetro[i]
	for block in g.tetro {
		y := block.y + g.pos_y + 1
		x := block.x + g.pos_x
		// Reached the bottom of the screen or another block?
		// TODO: if g.field[y][x] != 0
		row := g.field[y]
		if row[x] != 0 {
			// The new tetro has no space to drop => end of the game
			if g.pos_y < 2 {
				g.init_game()
				return
			}
			// Drop it and generate a new one
			g.drop_tetro()
			g.generate_tetro()
			return
		}
	}
	g.pos_y++
}

fn (g mut Game) move_right(dx int) {
	// Reached left/right edge or another tetro? 
	for i := 0; i < TETRO_SIZE; i++ {
		tetro := g.tetro[i]
		y := tetro.y + g.pos_y
		x := tetro.x + g.pos_x + dx
		row := g.field[y]
		if row[x] != 0 {
			// Do not move
			return
		}
	}
	g.pos_x += dx
}

fn (g mut Game) delete_completed_lines() {
	for y := FIELD_HEIGHT; y >= 1; y-- {
		g.delete_completed_line(y)
	}
}

fn (g mut Game) delete_completed_line(y int) {
	for x := 1; x <= FIELD_WIDTH; x++ {
		f := g.field[y]
		if f[x] == 0 {
			return
		}
	}
	// Move everything down by 1 position
	for yy := y - 1; yy >= 1; yy-- {
		for x := 1; x <= FIELD_WIDTH; x++ {
			mut a := g.field[yy + 1]
			mut b := g.field[yy]
			a[x] = b[x]
		}
	}
}

// Place a new tetro on top
fn (g mut Game) generate_tetro() {
	g.pos_y = 0
	g.pos_x = FIELD_WIDTH / 2 - TETRO_SIZE / 2
	g.tetro_idx = rand.next(B_TETROS.len)
	g.rotation_idx = 0 
	g.get_tetro() 
}

// Get the right tetro from cache 
fn (g mut Game) get_tetro() {
	idx := g.tetro_idx * TETRO_SIZE * TETRO_SIZE + g.rotation_idx * TETRO_SIZE 
	g.tetro = g.tetros_cache.slice(idx, idx + TETRO_SIZE) 
} 

fn (g mut Game) drop_tetro() {
	for i := 0; i < TETRO_SIZE; i++ {
		tetro := g.tetro[i]
		x := tetro.x + g.pos_x
		y := tetro.y + g.pos_y
		// Remember the color of each block
		// TODO: g.field[y][x] = g.tetro_idx + 1
		mut row := g.field[y]
		row[x] = g.tetro_idx + 1
	}
}

fn (g &Game) draw_tetro() {
	for i := 0; i < TETRO_SIZE; i++ {
		tetro := g.tetro[i]
		g.draw_block(g.pos_y + tetro.y, g.pos_x + tetro.x, g.tetro_idx + 1)
	}
}

fn (g &Game) draw_block(i, j int, color_idx int) {
	g.gg.draw_rect((j - 1) * BLOCK_SIZE, (i - 1) * BLOCK_SIZE, 
		BLOCK_SIZE - 1, BLOCK_SIZE - 1, COLORS[color_idx])
}

fn (g &Game) draw_field() {
	for i := 1; i < FIELD_HEIGHT + 1; i++ {
		for j := 1; j < FIELD_WIDTH + 1; j++ {
			f := g.field[i]
			if f[j] > 0 {
				g.draw_block(i, j, f[j])
			}
		}
	}
}

fn (g &Game) draw_scene() {
	g.draw_tetro()
	g.draw_field()
}

fn parse_binary_tetro(t int) []Block {
	res := [Block{} ; 4]
	mut cnt := 0
	horizontal := t == 9// special case for the horizontal line
	for i := 0; i <= 3; i++ {
		// Get ith digit of t
		p := int(math.pow(10, 3 - i))
		mut digit := int(t / p)
		t %= p
		// Convert the digit to binary
		for j := 3; j >= 0; j-- {
			bin := digit % 2
			digit /= 2
			if bin == 1 || (horizontal && i == TETRO_SIZE - 1) {
				// TODO: res[cnt].x = j
				// res[cnt].y = i
				mut point := &res[cnt]
				point.x = j
				point.y = i
				cnt++
			}
		}
	}
	return res
}

// TODO: this exposes the unsafe C interface, clean up
fn key_down(wnd voidptr, key int, code int, action, mods int) {
	if action != 2 && action != 1 {
		return
	}
	// Fetch the game object stored in the user pointer
	mut game := &Game(glfw.get_window_user_pointer(wnd))
	switch key {
	case GLFW_KEY_UP:
		// Rotate the tetro
		game.rotation_idx++
		if game.rotation_idx == TETRO_SIZE {
			game.rotation_idx = 0
		}
		game.get_tetro() 
		if game.pos_x < 0 {
			game.pos_x = 1
		}
	case GLFW_KEY_LEFT:
		game.move_right(-1)
	case GLFW_KEY_RIGHT:
		game.move_right(1)
	case GLFW_KEY_DOWN:
		game.move_tetro() // drop faster when the player presses <down>
	}
}

