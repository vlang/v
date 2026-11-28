// Port of raylib [shaders] example - game of life.
// PORT NOTE: shader game_of_life.fs with GPU ping-pong render textures requires gg user-shader
// support (not yet available); a CPU Conway simulation is rendered with gg primitives and a
// RenderTexture2D-sized grid.
module main

import gg
import rand

const win_width = 800
const win_height = 450
const grid_w = 200
const grid_h = 110

struct App {
mut:
	gg     &gg.Context = unsafe { nil }
	grid   []bool
	next   []bool
	paused bool
	speed  int = 1
	frame  int
}

fn seed(mut app &App) {
	for i in 0 .. app.grid.len {
		app.grid[i] = rand.intn(12) or { 0 } == 0
	}
}

fn step(mut app &App) {
	for y in 0 .. grid_h {
		for x in 0 .. grid_w {
			mut n := 0
			for dy in -1 .. 2 {
				for dx in -1 .. 2 {
					if dx == 0 && dy == 0 {
						continue
					}
					nx := (x + dx + grid_w) % grid_w
					ny := (y + dy + grid_h) % grid_h
					if app.grid[ny * grid_w + nx] {
						n++
					}
				}
			}
			idx := y * grid_w + x
			app.next[idx] = if app.grid[idx] { n == 2 || n == 3 } else { n == 3 }
		}
	}
	tmp := app.grid
	app.grid = app.next
	app.next = tmp
}

fn main() {
	mut app := &App{}
	app.grid = []bool{len: grid_w * grid_h, init: false}
	app.next = []bool{len: grid_w * grid_h, init: false}
	seed(mut app)
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - game of life'
		bg_color:      gg.white
		user_data:     app
		event_fn:      event
		update_fn:     update
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn event(e &gg.Event, data voidptr) {
	if e.typ != .key_down {
		return
	}
	mut app := unsafe { &App(data) }
	match e.key_code {
		.space { app.paused = !app.paused }
		.up { app.speed++ }
		.down {
			app.speed--
			if app.speed < 1 {
				app.speed = 1
			}
		}
		.r { seed(mut app) }
		else {}
	}
}

fn update(dt f32, data voidptr) {
	_ = dt
	mut app := unsafe { &App(data) }
	if app.paused {
		return
	}
	app.frame++
	if app.frame % app.speed == 0 {
		step(mut app)
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	cw := f32(win_width) / f32(grid_w)
	ch := f32(win_height) / f32(grid_h)
	for y in 0 .. grid_h {
		for x in 0 .. grid_w {
			if app.grid[y * grid_w + x] {
				ctx.draw_rect_filled(f32(x) * cw, f32(y) * ch, cw, ch, gg.dark_blue)
			}
		}
	}
	ctx.draw_text(10, 10, 'Game of Life (CPU approximation, shader/RT ping-pong unavailable)',
		size:  14
		color: gg.red
	)
	ctx.draw_text(10, 30, 'SPACE pause, UP/DOWN speed, R reseed', size: 14, color: gg.red)
	ctx.end()
}
