// Port of raylib [shaders] example - rlgl compute (Game of Life on the GPU).
// PORT NOTE: OpenGL 4.3 compute shaders / SSBOs require gg user-shader support (not yet
// available); the same Game of Life is simulated on the CPU and drawn with gg primitives.
module main

import gg
import rand

const grid_size = 96
const cell_size = 6

struct App {
mut:
	gg     &gg.Context = unsafe { nil }
	grid   []bool
	next   []bool
	brush  int = 8
	paused bool
}

fn main() {
	mut app := &App{}
	app.grid = []bool{len: grid_size * grid_size, init: false}
	app.next = []bool{len: grid_size * grid_size, init: false}
	for i in 0 .. app.grid.len {
		app.grid[i] = rand.intn(5) or { 0 } == 0
	}
	app.gg = gg.new_context(
		width:         grid_size * cell_size
		height:        grid_size * cell_size
		window_title:  'raylib [shaders] example - rlgl compute'
		bg_color:      gg.white
		user_data:     app
		event_fn:      event
		update_fn:     update
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn step(mut app &App) {
	for y in 0 .. grid_size {
		for x in 0 .. grid_size {
			mut neighbors := 0
			for dy in -1 .. 2 {
				for dx in -1 .. 2 {
					if dx == 0 && dy == 0 {
						continue
					}
					nx := (x + dx + grid_size) % grid_size
					ny := (y + dy + grid_size) % grid_size
					if app.grid[ny * grid_size + nx] {
						neighbors++
					}
				}
			}
			idx := y * grid_size + x
			app.next[idx] = if app.grid[idx] {
				neighbors == 2 || neighbors == 3
			} else {
				neighbors == 3
			}
		}
	}
	tmp := app.grid
	app.grid = app.next
	app.next = tmp
}

fn event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.paused = !app.paused
	}
	if e.typ == .mouse_down || e.typ == .mouse_move {
		cx := int(e.mouse_x) / cell_size
		cy := int(e.mouse_y) / cell_size
		if cx >= 0 && cx < grid_size && cy >= 0 && cy < grid_size {
			for dy in -app.brush / 2 .. app.brush / 2 {
				for dx in -app.brush / 2 .. app.brush / 2 {
					nx := cx + dx
					ny := cy + dy
					if nx >= 0 && nx < grid_size && ny >= 0 && ny < grid_size {
						app.grid[ny * grid_size + nx] = true
					}
				}
			}
		}
	}
}

fn update(dt f32, data voidptr) {
	_ = dt
	mut app := unsafe { &App(data) }
	if !app.paused {
		step(app)
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	for y in 0 .. grid_size {
		for x in 0 .. grid_size {
			if app.grid[y * grid_size + x] {
				ctx.draw_rect_filled(f32(x * cell_size), f32(y * cell_size), f32(cell_size),
					f32(cell_size), gg.dark_green)
			}
		}
	}
	ctx.draw_text(10, 10, 'CPU Game of Life (compute shader unavailable in gg)',
		size:  14
		color: gg.red
	)
	ctx.end()
}
