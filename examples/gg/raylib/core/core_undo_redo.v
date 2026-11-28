module main

// raylib [core] example - undo redo
// A ring buffer of player-state snapshots provides undo/redo with CTRL+Z/Y.
// Arrows move, SPACE changes the player's colour.

import gg
import rand

const screen_width = 800
const screen_height = 450
const max_states = 26
const cell_size = 24
const grid_cells_x = 30
const grid_cells_y = 13

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const sky_blue = gg.Color{102, 191, 255, 255}
const blue = gg.Color{0, 121, 241, 255}
const green = gg.Color{0, 158, 47, 255}
const lime = gg.Color{124, 252, 0, 255}
const gold = gg.Color{255, 203, 0, 255}
const orange = gg.Color{255, 161, 0, 255}

struct State {
mut:
	x     int
	y     int
	color gg.Color
}

struct App {
mut:
	gg          &gg.Context = unsafe { nil }
	player      State       = State{10, 10, gg.red}
	states      [max_states]State
	current     int
	first       int
	last        int
	frame_count int
	ctrl_down   bool
}

fn main() {
	mut app := &App{}
	for i in 0 .. max_states {
		app.states[i] = State{10, 10, gg.red}
	}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - undo redo'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn same(a State, b State) bool {
	return a.x == b.x && a.y == b.y && a.color == b.color
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down {
		if e.key_code == .left_control {
			app.ctrl_down = true
		}
		if !e.key_repeat {
			match e.key_code {
				.right { app.player.x++ }
				.left { app.player.x-- }
				.up { app.player.y-- }
				.down { app.player.y++ }
				.space {
					app.player.color = gg.Color{u8(rand.int_in_range(20, 255) or { 100 }), u8(rand.int_in_range(20, 220) or { 100 }), u8(rand.int_in_range(20, 240) or { 100 }), 255}
				}
				.z {
					if app.ctrl_down && app.current != app.first {
						app.current--
						if app.current < 0 {
							app.current = max_states - 1
						}
						app.player = app.states[app.current]
					}
				}
				.y {
					if app.ctrl_down && app.current != app.last {
						next := (app.current + 1) % max_states
						if next != app.first {
							app.current = next
							app.player = app.states[app.current]
						}
					}
				}
				else {}
			}
		}
	}
	if e.typ == .key_up && e.key_code == .left_control {
		app.ctrl_down = false
	}
	if app.player.x < 0 {
		app.player.x = 0
	}
	if app.player.x >= grid_cells_x {
		app.player.x = grid_cells_x - 1
	}
	if app.player.y < 0 {
		app.player.y = 0
	}
	if app.player.y >= grid_cells_y {
		app.player.y = grid_cells_y - 1
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	app.frame_count++
	if app.frame_count >= 2 {
		if !same(app.states[app.current], app.player) {
			app.current++
			if app.current >= max_states {
				app.current = 0
			}
			if app.current == app.first {
				app.first++
				if app.first >= max_states {
					app.first = 0
				}
			}
			app.states[app.current] = State{app.player.x, app.player.y, app.player.color}
			app.last = app.current
		}
		app.frame_count = 0
	}

	ctx.begin()
	ctx.draw_text(40, 20, '[ARROWS] MOVE PLAYER - [SPACE] CHANGE PLAYER COLOR',
		size:  20
		color: dark_gray
	)
	gx, gy := f32(40), f32(60)
	mut i := app.first
	for i < app.current {
		ctx.draw_rect(
			x:     gx + f32(app.states[i].x) * cell_size
			y:     gy + f32(app.states[i].y) * cell_size
			w:     cell_size
			h:     cell_size
			color: light_gray
		)
		i++
	}
	mut y := 0
	for y <= grid_cells_y {
		ctx.draw_line(gx, gy + f32(y) * cell_size,
			gx + f32(grid_cells_x) * cell_size, gy + f32(y) * cell_size, gray)
		y++
	}
	mut x := 0
	for x <= grid_cells_x {
		ctx.draw_line(gx + f32(x) * cell_size, gy,
			gx + f32(x) * cell_size, gy + f32(grid_cells_y) * cell_size, gray)
		x++
	}
	ctx.draw_rect(
		x:     gx + f32(app.player.x) * cell_size
		y:     gy + f32(app.player.y) * cell_size
		w:     cell_size + 1
		h:     cell_size + 1
		color: app.player.color
	)

	bx, by := f32(110), f32(400)
	ctx.draw_rect(x: bx + 8 + f32(app.current) * cell_size, y: by - 10, w: 8, h: 8, color: gg.red)
	ctx.draw_rect_empty(bx + 2 + f32(app.first) * cell_size, by + 27, 8, 8, gg.black)
	ctx.draw_rect_empty(bx + 14 + f32(app.last) * cell_size, by + 27, 8, 8, gg.black)
	i = 0
	for i < max_states {
		c := if i <= app.last && i >= app.first { sky_blue } else { light_gray }
		ctx.draw_rect(x: bx + f32(i) * cell_size, y: by, w: cell_size, h: cell_size, color: c)
		ctx.draw_rect_empty(bx + f32(i) * cell_size, by, cell_size, cell_size, gray)
		i++
	}
	ctx.draw_rect(
		x:     bx + f32(app.current) * cell_size
		y:     by
		w:     cell_size
		h:     cell_size
		color: gold
	)
	ctx.draw_rect_empty(bx + f32(app.current) * cell_size, by, cell_size, cell_size, orange)
	ctx.draw_text(25, 409, 'UNDO STATES:', size: 10, color: dark_gray)
	ctx.end()
}
