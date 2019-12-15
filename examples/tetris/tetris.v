// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import rand
import time
import gx
import gg
import glfw
import math
import freetype

const (
	BlockSize = 20 // pixels
	FieldHeight = 20 // # of blocks
	FieldWidth = 10
	TetroSize = 4
	WinWidth = BlockSize * FieldWidth
	WinHeight = BlockSize * FieldHeight
	TimerPeriod = 250 // ms
	TextSize = 12
	LimitThickness = 3
)

const (
	text_cfg = gx.TextCfg{
		align:gx.ALIGN_LEFT
		size:TextSize
		color:gx.rgb(0, 0, 0)
	}
	over_cfg = gx.TextCfg{
		align:gx.ALIGN_LEFT
		size:TextSize
		color:gx.White
	}
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
		gx.rgb(0, 0, 0),        // unused ?
		gx.rgb(255, 242, 0),    // yellow quad
		gx.rgb(174, 0, 255),    // purple triple
		gx.rgb(60, 255, 0),     // green short topright
		gx.rgb(255, 0, 0),      // red short topleft
		gx.rgb(255, 180, 31),   // orange long topleft
		gx.rgb(33, 66, 255),    // blue long topright
		gx.rgb(74, 198, 255),   // lightblue longest
		gx.rgb(0, 170, 170),    // unused ?
	]

	BackgroundColor = gx.White
	UIColor = gx.Red
)

// TODO: type Tetro [TetroSize]struct{ x, y int }
struct Block {
	mut:
	x int
	y int
}

enum GameState {
        paused running gameover
}
struct Game {
	mut:
	// Score of the current game
	score        int
	// State of the current game
	state    GameState
	// Position of the current tetro
	pos_x        int
	pos_y        int
	// field[y][x] contains the color of the block with (x,y) coordinates
	// "-1" border is to avoid bounds checking.
	// -1 -1 -1 -1
	// -1  0  0 -1
	// -1  0  0 -1
	// -1 -1 -1 -1
	field       [][]int
	// TODO: tetro Tetro
	tetro       []Block
	// TODO: tetros_cache []Tetro
	tetros_cache []Block
	// Index of the current tetro. Refers to its color.
	tetro_idx    int
	// Index of the rotation (0-3)
	rotation_idx int
	// gg context for drawing
	gg          &gg.GG
	// ft context for font drawing
	ft          &freetype.FreeType
	font_loaded bool
}

fn main() {
	glfw.init_glfw()
	mut game := &Game{
		gg: gg.new_context(gg.Cfg {
			width: WinWidth
			height: WinHeight
			use_ortho: true // This is needed for 2D drawing
			create_window: true
			window_title: 'V Tetris'
			window_user_ptr: game
		})
		ft: 0
	}
	game.gg.window.set_user_ptr(game) // TODO remove this when `window_user_ptr:` works
	game.init_game()
	game.gg.window.onkeydown(key_down)
	go game.run() // Run the game loop in a new thread
	gg.clear(BackgroundColor)
	// Try to load font
	game.ft = freetype.new_context(gg.Cfg{
		width: WinWidth
		height: WinHeight
		use_ortho: true
		font_size: 18
		scale: 2
		window_user_ptr: 0
	})
	game.font_loaded = (game.ft != 0 )
	for {
		gg.clear(BackgroundColor)
		game.draw_scene()
		game.gg.render()
		if game.gg.window.should_close() {
			game.gg.window.destroy()
			return
		}
	}
}

fn (g mut Game) init_game() {
	g.parse_tetros()
	rand.seed(time.now().uni)
	g.generate_tetro()
	g.field = [] // TODO: g.field = [][]int
	// Generate the field, fill it with 0's, add -1's on each edge
	for i := 0; i < FieldHeight + 2; i++ {
		mut row := [0].repeat(FieldWidth + 2)
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
	g.score = 0
	g.state = .running
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
		if g.state == .running {
			g.move_tetro()
			g.delete_completed_lines()
		}
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
		//if g.field[y][x] != 0 {
		row := g.field[y]
		if row[x] != 0 {
			// The new tetro has no space to drop => end of the game
			if g.pos_y < 2 {
				g.state = .gameover
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

fn (g mut Game) move_right(dx int) bool {
	// Reached left/right edge or another tetro?
	for i := 0; i < TetroSize; i++ {
		tetro := g.tetro[i]
		y := tetro.y + g.pos_y
		x := tetro.x + g.pos_x + dx
		row := g.field[y]
		if row[x] != 0 {
			// Do not move
			return false
		}
	}
	g.pos_x += dx
	return true
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
	g.score += 10
	// Move everything down by 1 position
	for yy := y - 1; yy >= 1; yy-- {
		for x := 1; x <= FieldWidth; x++ {
			mut a := g.field[yy + 1]
			b := g.field[yy]
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
	g.tetro = g.tetros_cache[idx..idx+TetroSize]
}

// TODO mut
fn (g &Game) drop_tetro() {
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

fn (g &Game) draw_block(i, j, color_idx int) {
	color := if g.state == .gameover { gx.Gray } else { Colors[color_idx] }
	g.gg.draw_rect((j - 1) * BlockSize, (i - 1) * BlockSize,
		BlockSize - 1, BlockSize - 1, color)
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

fn (g mut Game) draw_ui() {
	if g.font_loaded {
		g.ft.draw_text(1, 3, g.score.str(), text_cfg)
		if g.state == .gameover {
			g.gg.draw_rect(0, WinHeight / 2 - TextSize, WinWidth,
		 								5 * TextSize, UIColor)
			g.ft.draw_text(1, WinHeight / 2 + 0 * TextSize, 'Game Over', over_cfg)
			g.ft.draw_text(1, WinHeight / 2 + 2 * TextSize, 'Space to restart', over_cfg)
		} else if g.state == .paused {
			g.gg.draw_rect(0, WinHeight / 2 - TextSize, WinWidth,
				5 * TextSize, UIColor)
			g.ft.draw_text(1, WinHeight / 2 + 0 * TextSize, 'Game Paused', text_cfg)
			g.ft.draw_text(1, WinHeight / 2 + 2 * TextSize, 'SPACE to resume', text_cfg)
		}
	}
	//g.gg.draw_rect(0, BlockSize, WinWidth, LimitThickness, UIColor)
}

fn (g mut Game) draw_scene() {
	g.draw_tetro()
	g.draw_field()
	g.draw_ui()
}

fn parse_binary_tetro(t_ int) []Block {
	mut t := t_
	res := [Block{}].repeat(4)
	mut cnt := 0
	horizontal := t == 9// special case for the horizontal line
	for i := 0; i <= 3; i++ {
		// Get ith digit of t
		p := int(math.pow(10, 3 - i))
		mut digit := t / p
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
fn key_down(wnd voidptr, key, code, action, mods int) {
	if action != 2 && action != 1 {
		return
	}
	// Fetch the game object stored in the user pointer
	mut game := &Game(glfw.get_window_user_pointer(wnd))
	// global keys
	match key {
		glfw.KEY_ESCAPE {
			glfw.set_should_close(wnd, true)
		}
		glfw.key_space {
			if game.state == .running {
				game.state = .paused
			} else if game.state == .paused {
				game.state = .running
			} else if game.state == .gameover {
				game.init_game()
				game.state = .running
			}
		}
		else {}
	}

	if game.state != .running {
		return
	}
	// keys while game is running
	match key {
	glfw.KeyUp {
		// Rotate the tetro
		old_rotation_idx := game.rotation_idx
		game.rotation_idx++
		if game.rotation_idx == TetroSize {
			game.rotation_idx = 0
		}
		game.get_tetro()
		if !game.move_right(0) {
			game.rotation_idx = old_rotation_idx
			game.get_tetro()
		}
		if game.pos_x < 0 {
			//game.pos_x = 1
		}
	}
	glfw.KeyLeft {
		game.move_right(-1)
	}
	glfw.KeyRight {
		game.move_right(1)
	}
	glfw.KeyDown {
		game.move_tetro() // drop faster when the player presses <down>
	}
	else { }
	}
}
