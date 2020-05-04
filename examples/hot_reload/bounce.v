// Build this example with
// v -live bounce.v
module main

import gx
import gl
import gg
import glfw
import time

struct Game {
mut:
	gg       &gg.GG
	x        int
	y        int
	dy       int
	dx       int
	height   int
	width    int
	main_wnd &glfw.Window
	draw_fn  voidptr
}

const (
	window_width = 400
	window_height = 300
	width = 50
	red   = gx.rgb(255, 0, 0)
	green = gx.rgb(0, 255, 0)
	blue  = gx.rgb(0, 0, 255)
	black = gx.rgb(0, 0, 0)
)

fn main() {
	glfw.init_glfw()
	mut game := &Game{
		gg: 0
		dx: 2
		dy: 2
		height: window_height
		width: window_width
		main_wnd: 0
		draw_fn: 0
	}
	window := glfw.create_window(glfw.WinCfg{
		width: window_width
		height: window_height
		borderless: false
		title: 'Hot code reloading demo'
		ptr: game
		always_on_top: true
	})
	// window.onkeydown(key_down)
	game.main_wnd = window
	window.make_context_current()
	gg.init_gg()
	game.gg = gg.new_context(gg.Cfg{
		width: window_width
		height: window_height
		font_size: 20
		use_ortho: true
		window_user_ptr: 0
	})
	println('Starting the game loop...')
	go game.run()
	for {
		if window.should_close() {
			break
		}
		gl.clear()
		gl.clear_color(255, 255, 255, 255)
		game.draw()
		window.swap_buffers()
		glfw.wait_events()
	}
}


// Try uncommenting or changing the lines inside the live functions.
// Guess what will happen:
[live]
fn (game &Game) draw() {
	game.gg.draw_rect(game.x, game.y, width, width, blue)
	game.gg.draw_rect(window_width - width - game.x + 10, 200 - game.y + width, width, width, gx.rgb(228, 10, 55))
	game.gg.draw_rect(game.x - 25, 250 - game.y, width, width, gx.rgb(28, 240, 55))
}

[live]
fn (mut game Game) update_model() {
	speed := 2
	game.x += speed * game.dx
	game.y += speed * game.dy
	if game.y >= game.height - width || game.y <= 0 {
		game.dy = -game.dy
	}
	if game.x >= game.width - width || game.x <= 0 {
		game.dx = -game.dx
	}
}

fn (mut game Game) run() {
	for {
		game.update_model()
		glfw.post_empty_event() // Refresh
		time.sleep_ms(17) // 60fps
	}
}









