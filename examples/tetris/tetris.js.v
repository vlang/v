// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import rand
import time
import gx
import gg
// import sokol.sapp

const (
	block_size      = 20 // virtual pixels
	field_height    = 20 // # of blocks
	field_width     = 10
	tetro_size      = 4
	win_width       = block_size * field_width
	win_height      = block_size * field_height
	timer_period    = 250 // ms
	text_size       = 24
	limit_thickness = 3
)

const (
	text_cfg = gx.TextCfg{
		align: .left
		size: text_size
		color: gx.rgb(0, 0, 0)
	}
	over_cfg = gx.TextCfg{
		align: .left
		size: text_size
		color: gx.white
	}
)

const (
	// Tetros' 4 possible states are encoded in binaries
	// 0000 0  0000 0  0000 0  0000 0  0000 0  0000 0
	// 0000 0  0000 0  0000 0  0000 0  0011 3  0011 3
	// 0110 6  0010 2  0011 3  0110 6  0001 1  0010 2
	// 0110 6  0111 7  0110 6  0011 3  0001 1  0010 2
	// There is a special case 1111, since 15 can't be used.
	b_tetros         = [
		[66, 66, 66, 66],
		[27, 131, 72, 232],
		[36, 231, 36, 231],
		[63, 132, 63, 132],
		[311, 17, 223, 74],
		[322, 71, 113, 47],
		[1111, 9, 1111, 9],
	]
	// Each tetro has its unique color
	colors           = [
		gx.rgb(0, 0, 0), /* unused ? */
		gx.rgb(255, 242, 0), /* yellow quad */
		gx.rgb(174, 0, 255), /* purple triple */
		gx.rgb(60, 255, 0), /* green short topright */
		gx.rgb(255, 0, 0), /* red short topleft */
		gx.rgb(255, 180, 31), /* orange long topleft */
		gx.rgb(33, 66, 255), /* blue long topright */
		gx.rgb(74, 198, 255), /* lightblue longest */
		gx.rgb(0, 170, 170),
	]
	background_color = gx.white
	ui_color         = gx.rgba(255, 0, 0, 210)
)

// TODO: type Tetro [tetro_size]struct{ x, y int }
struct Block {
mut:
	x int
	y int
}

enum GameState {
	paused
	running
	gameover
}

struct Game {
mut:
	// Score of the current game
	score int
	// Lines of the current game
	lines int
	// State of the current game
	state GameState
	// Block size in screen dimensions
	block_size int = block_size
	// Field margin
	margin int
	// Position of the current tetro
	pos_x int
	pos_y int
	// field[y][x] contains the color of the block with (x,y) coordinates
	// "-1" border is to avoid bounds checking.
	// -1 -1 -1 -1
	// -1  0  0 -1
	// -1  0  0 -1
	// -1 -1 -1 -1
	field [][]int
	// TODO: tetro Tetro
	tetro []Block
	// TODO: tetros_cache []Tetro
	tetros_cache []Block
	// Index of the current tetro. Refers to its color.
	tetro_idx int
	// Idem for the next tetro
	next_tetro_idx int
	// Index of the rotation (0-3)
	rotation_idx int
	// gg context for drawing
	gg          &gg.Context = unsafe { nil }
	font_loaded bool
	show_ghost  bool = true
	// frame/time counters:
	frame     int
	frame_old int
	frame_sw  time.StopWatch = time.new_stopwatch()
	second_sw time.StopWatch = time.new_stopwatch()
}

fn remap(v f32, min f32, max f32, new_min f32, new_max f32) f32 {
	return (((v - min) * (new_max - new_min)) / (max - min)) + new_min
}

[if showfps ?]
fn (mut game Game) showfps() {
	game.frame++
	last_frame_ms := f64(game.frame_sw.elapsed().microseconds()) / 1000.0
	ticks := f64(game.second_sw.elapsed().microseconds()) / 1000.0
	if ticks > 999.0 {
		fps := f64(game.frame - game.frame_old) * ticks / 1000.0
		$if debug {
			eprintln('fps: ${fps:5.1f} | last frame took: ${last_frame_ms:6.3f}ms | frame: ${game.frame:6} ')
		}
		game.second_sw.restart()
		game.frame_old = game.frame
	}
}

fn frame(mut game Game) {
	if game.gg.frame & 15 == 0 {
		game.update_game_state()
	}
	ws := gg.window_size()
	bs := remap(block_size, 0, win_height, 0, ws.height)
	m := (f32(ws.width) - bs * field_width) * 0.5
	game.block_size = int(bs)
	game.margin = int(m)
	game.frame_sw.restart()
	game.gg.begin()
	game.draw_scene()
	game.showfps()
	game.gg.end()
}

fn main() {
	mut game := &Game{
		gg: &gg.Context{}
	}

	game.gg = gg.new_context(
		bg_color: gx.white
		width: win_width
		height: win_height
		create_window: true
		window_title: 'V Tetris' //
		user_data: game
		frame_fn: frame
		event_fn: on_event
		canvas: 'canvas'
	)
	game.init_game()
	game.gg.run() // Run the render loop in the main thread
}

fn (mut g Game) init_game() {
	g.parse_tetros()
	g.next_tetro_idx = rand.intn(b_tetros.len) or { 0 } // generate initial "next"
	g.generate_tetro()
	g.field = []
	// Generate the field, fill it with 0's, add -1's on each edge
	for _ in 0 .. field_height + 2 {
		mut row := [0].repeat(field_width + 2)
		row[0] = -1
		row[field_width + 1] = -1
		g.field << row
	}
	for j in 0 .. field_width + 2 {
		g.field[0][j] = -1
		g.field[field_height + 1][j] = -1
	}
	g.score = 0
	g.lines = 0
	g.state = .running
}

fn (mut g Game) parse_tetros() {
	for b_tetros0 in b_tetros {
		for b_tetro in b_tetros0 {
			for t in parse_binary_tetro(b_tetro) {
				g.tetros_cache << t
			}
		}
	}
}

fn (mut g Game) update_game_state() {
	if g.state == .running {
		g.move_tetro()
		g.delete_completed_lines()
	}
}

fn (mut g Game) draw_ghost() {
	if g.state != .gameover && g.show_ghost {
		pos_y := g.move_ghost()
		for i in 0 .. tetro_size {
			tetro := g.tetro[i]
			g.draw_block_color(pos_y + tetro.y, g.pos_x + tetro.x, gx.rgba(125, 125, 225,
				40))
		}
	}
}

fn (g Game) move_ghost() int {
	mut pos_y := g.pos_y
	mut end := false
	for !end {
		for block in g.tetro {
			y := block.y + pos_y + 1
			x := block.x + g.pos_x
			if g.field[y][x] != 0 {
				end = true
				break
			}
		}
		pos_y++
	}
	return pos_y - 1
}

fn (mut g Game) move_tetro() bool {
	// Check each block in current tetro
	for block in g.tetro {
		y := block.y + g.pos_y + 1
		x := block.x + g.pos_x
		// Reached the bottom of the screen or another block?
		if g.field[y][x] != 0 {
			// The new tetro has no space to drop => end of the game
			if g.pos_y < 2 {
				g.state = .gameover
				return false
			}
			// Drop it and generate a new one
			g.drop_tetro()
			g.generate_tetro()
			return false
		}
	}
	g.pos_y++
	return true
}

fn (mut g Game) move_right(dx int) bool {
	// Reached left/right edge or another tetro?
	for i in 0 .. tetro_size {
		tetro := g.tetro[i]
		y := tetro.y + g.pos_y
		x := tetro.x + g.pos_x + dx
		if g.field[y][x] != 0 {
			// Do not move
			return false
		}
	}
	g.pos_x += dx
	return true
}

fn (mut g Game) delete_completed_lines() {
	for y := field_height; y >= 1; y-- {
		g.delete_completed_line(y)
	}
}

fn (mut g Game) delete_completed_line(y int) {
	for x := 1; x <= field_width; x++ {
		if g.field[y][x] == 0 {
			return
		}
	}
	g.score += 10
	g.lines++
	// Move everything down by 1 position
	for yy := y - 1; yy >= 1; yy-- {
		for x := 1; x <= field_width; x++ {
			g.field[yy + 1][x] = g.field[yy][x]
		}
	}
}

// Place a new tetro on top
fn (mut g Game) generate_tetro() {
	g.pos_y = 0
	g.pos_x = field_width / 2 - tetro_size / 2
	g.tetro_idx = g.next_tetro_idx
	g.next_tetro_idx = rand.intn(b_tetros.len) or { 0 }
	g.rotation_idx = 0
	g.get_tetro()
}

// Get the right tetro from cache
fn (mut g Game) get_tetro() {
	idx := g.tetro_idx * tetro_size * tetro_size + g.rotation_idx * tetro_size
	mut tetros := []Block{}
	for tetro in g.tetros_cache[idx..idx + tetro_size] {
		tetros << Block{tetro.x, tetro.y}
	}
	g.tetro = tetros
	// g.tetro = g.tetros_cache[idx..idx + tetro_size].clone()
}

// TODO mut
fn (mut g Game) drop_tetro() {
	for i in 0 .. tetro_size {
		tetro := g.tetro[i]
		x := tetro.x + g.pos_x
		y := tetro.y + g.pos_y
		// Remember the color of each block
		g.field[y][x] = g.tetro_idx + 1
	}
}

fn (mut g Game) draw_tetro() {
	for i in 0 .. tetro_size {
		tetro := g.tetro[i]
		g.draw_block(g.pos_y + tetro.y, g.pos_x + tetro.x, g.tetro_idx + 1)
	}
}

fn (mut g Game) draw_next_tetro() {
	if g.state != .gameover {
		idx := g.next_tetro_idx * tetro_size * tetro_size
		next_tetro := g.tetros_cache[idx..idx + tetro_size]
		pos_y := 0
		pos_x := field_width / 2 - tetro_size / 2
		for i in 0 .. tetro_size {
			block := next_tetro[i]
			g.draw_block_color(pos_y + block.y, pos_x + block.x, gx.rgb(220, 220, 220))
		}
	}
}

fn (mut g Game) draw_block_color(i int, j int, color gx.Color) {
	g.gg.draw_rect(f32((j - 1) * g.block_size) + g.margin, f32((i - 1) * g.block_size),
		f32(g.block_size - 1), f32(g.block_size - 1), color)
}

fn (mut g Game) draw_block(i int, j int, color_idx int) {
	color := if g.state == .gameover { gx.gray } else { colors[color_idx] }
	g.draw_block_color(i, j, color)
}

fn (mut g Game) draw_field() {
	for i := 1; i < field_height + 1; i++ {
		for j := 1; j < field_width + 1; j++ {
			if g.field[i][j] > 0 {
				g.draw_block(i, j, g.field[i][j])
			}
		}
	}
}

fn (mut g Game) draw_ui() {
	ws := gg.window_size()
	textsize := int(remap(text_size, 0, win_width, 0, ws.width))
	g.gg.draw_text(1, 10, g.score.str(), text_cfg)
	lines := g.lines.str()
	g.gg.draw_text(ws.width - lines.len * textsize, 10, lines, text_cfg)
	if g.state == .gameover {
		g.gg.draw_rect(0, ws.height / 2 - textsize, ws.width, 5 * textsize, ui_color)
		g.gg.draw_text(1, ws.height / 2 + 0 * textsize, 'Game Over', over_cfg)
		g.gg.draw_text(1, ws.height / 2 + 2 * textsize, 'Space to restart', over_cfg)
	} else if g.state == .paused {
		g.gg.draw_rect(0, ws.height / 2 - textsize, ws.width, 5 * textsize, ui_color)
		g.gg.draw_text(1, ws.height / 2 + 0 * textsize, 'Game Paused', text_cfg)
		g.gg.draw_text(1, ws.height / 2 + 2 * textsize, 'SPACE to resume', text_cfg)
	}
	// g.gg.draw_rect(0, block_size, win_width, limit_thickness, ui_color)
}

fn (mut g Game) draw_scene() {
	g.draw_ghost()
	g.draw_next_tetro()
	g.draw_tetro()
	g.draw_field()
	g.draw_ui()
}

fn parse_binary_tetro(t_ int) []Block {
	mut t := t_
	mut res := [Block{}, Block{}, Block{}, Block{}]
	mut cnt := 0
	horizontal := t == 9 // special case for the horizontal line
	ten_powers := [1000, 100, 10, 1]
	for i := 0; i <= 3; i++ {
		// Get ith digit of t
		p := ten_powers[i]
		mut digit := t / p
		t %= p

		// Convert the digit to binary
		for j := 3; j >= 0; j-- {
			bin := digit % 2
			digit /= 2
			if bin == 1 || (horizontal && i == tetro_size - 1) {
				res[cnt].x = j
				res[cnt].y = i
				cnt++
			}
		}
	}
	return res
}

fn on_event(e &gg.Event, mut game Game) {
	// println('code=$e.char_code')
	if e.typ == .key_down {
		game.key_down(e.key_code)
	}
}

fn (mut game Game) rotate_tetro() {
	old_rotation_idx := game.rotation_idx
	game.rotation_idx++
	if game.rotation_idx == tetro_size {
		game.rotation_idx = 0
	}
	game.get_tetro()
	if !game.move_right(0) {
		game.rotation_idx = old_rotation_idx
		game.get_tetro()
	}
	if game.pos_x < 0 {
		// game.pos_x = 1
	}
}

fn (mut game Game) key_down(key gg.KeyCode) {
	// global keys
	match key {
		.escape {
			game.gg.quit()
		}
		.space {
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
		.up {
			// Rotate the tetro
			game.rotate_tetro()
		}
		.left {
			game.move_right(-1)
		}
		.right {
			game.move_right(1)
		}
		.down {
			game.move_tetro() // drop faster when the player presses <down>
		}
		.d {
			for game.move_tetro() {
			}
		}
		.g {
			game.show_ghost = !game.show_ghost
		}
		else {}
	}
}
