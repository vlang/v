// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import rand
import time
import gx
import gl
import gg
import glfw
import math

const (
	BlockSize = 20 // pixels
	FieldHeight = 20 // # of blocks
	FieldWidth = 10
	TetroSize = 4
	WinWidth = BlockSize * FieldWidth
	WinHeight = BlockSize * FieldHeight
	TimerPeriod = 250 // ms
)

const (
	// Tetros' 4 possible states are encoded in binaries
	BTetros = [
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
	Colors = [
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

// TODO: type Tetro [TetroSize]struct{ x, y int }
struct Block {
	mut:
	x int
	y int
}

struct Game {
	mut:
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
	glfw.init()
	mut game := &Game{gg: 0} // TODO
	game.parse_tetros() 
	game.init_game()
	mut window := glfw.create_window(glfw.WinCfg {
		width: WinWidth
		height: WinHeight
		title: 'V Tetris'
		ptr: game // glfw user pointer
	})
	window.make_context_current()
	window.onkeydown(key_down)
	gg.init()
	game.gg = gg.new_context(gg.Cfg {
		width: WinWidth
		height: WinHeight
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
	for i := 0; i < FieldHeight + 2; i++ {
		mut row := [0; FieldWidth + 2]
		row[0] = - 1
		row[FieldWidth + 1] = - 1
		g.field << row
	}
	mut first_row := g.field[0]
	mut last_row := g.field[FieldHeight + 1]
	for j := 0; j < FieldWidth + 2; j++ {
		first_row[j] = - 1
		last_row[j] = - 1
	}
}

fn (g mut Game) parse_tetros() {
	for b_tetros in BTetros {
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
		time.sleep_ms(TimerPeriod)
	}
}

fn (g mut Game) move_tetro() {
	// Check each block in current tetro
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
	for i := 0; i < TetroSize; i++ {
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
	for y := FieldHeight; y >= 1; y-- {
		g.delete_completed_line(y)
	}
}

fn (g mut Game) delete_completed_line(y int) {
	for x := 1; x <= FieldWidth; x++ {
		f := g.field[y]
		if f[x] == 0 {
			return
		}
	}
	// Move everything down by 1 position
	for yy := y - 1; yy >= 1; yy-- {
		for x := 1; x <= FieldWidth; x++ {
			mut a := g.field[yy + 1]
			mut b := g.field[yy]
			a[x] = b[x]
		}
	}
}

// Place a new tetro on top
fn (g mut Game) generate_tetro() {
	g.pos_y = 0
	g.pos_x = FieldWidth / 2 - TetroSize / 2
	g.tetro_idx = rand.next(BTetros.len)
	g.rotation_idx = 0 
	g.get_tetro() 
}

// Get the right tetro from cache 
fn (g mut Game) get_tetro() {
	idx := g.tetro_idx * TetroSize * TetroSize + g.rotation_idx * TetroSize 
	g.tetro = g.tetros_cache.slice(idx, idx + TetroSize) 
} 

fn (g mut Game) drop_tetro() {
	for i := 0; i < TetroSize; i++ {
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
	for i := 0; i < TetroSize; i++ {
		tetro := g.tetro[i]
		g.draw_block(g.pos_y + tetro.y, g.pos_x + tetro.x, g.tetro_idx + 1)
	}
}

fn (g &Game) draw_block(i, j int, color_idx int) {
	g.gg.draw_rect((j - 1) * BlockSize, (i - 1) * BlockSize, 
		BlockSize - 1, BlockSize - 1, Colors[color_idx])
}

fn (g &Game) draw_field() {
	for i := 1; i < FieldHeight + 1; i++ {
		for j := 1; j < FieldWidth + 1; j++ {
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
			if bin == 1 || (horizontal && i == TetroSize - 1) {
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
	case glfw.KeyUp:
		// Rotate the tetro
		game.rotation_idx++
		if game.rotation_idx == TetroSize {
			game.rotation_idx = 0
		}
		game.get_tetro() 
		if game.pos_x < 0 {
			game.pos_x = 1
		}
	case glfw.KeyLeft:
		game.move_right(-1)
	case glfw.KeyRight:
		game.move_right(1)
	case glfw.KeyDown:
		game.move_tetro() // drop faster when the player presses <down>
	}
}

