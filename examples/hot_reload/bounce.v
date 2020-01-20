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

fn main() {
	glfw.init_glfw()
	width := 600
	height := 300
	mut game := &Game{
		gg: 0
		dx: 2
		dy: 2
		height: height
		width: width
		main_wnd: 0
		draw_fn: 0
	}
	window := glfw.create_window(glfw.WinCfg {
		width: width
		height: height
		borderless: false
		title: 'Hot code reloading demo'
		ptr: game
		always_on_top: true
	})
	//window.onkeydown(key_down)
	game.main_wnd = window
	window.make_context_current()
	gg.init_gg()
	game.gg = gg.new_context(gg.Cfg {
		width: width
		height: height
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

const (
	width = 50
	red   = gx.rgb(255,0,0)
	green = gx.rgb(0,255,0)
	blue  = gx.rgb(0,0,255)
)

// Try uncommenting or changing the lines inside the live functions.
// Guess what will happen:
[live]
fn (game &Game) draw() {
	game.gg.draw_rect(game.x, game.y, width, width, blue)
	//	game.gg.draw_rect(game.x, game.y, width, width, gx.rgb(128,10,255))
}

[live]
fn (game mut Game) update_model() {
//    game.x = 0 game.y = 0 game.dx = 1 game.dy = 1
//    game.dx = 3 game.dy = 3
	speed := 2
	game.x += speed * game.dx
	game.y += speed * game.dy 
	if game.y >= game.height - width || game.y <= 0 {
		game.dy = - game.dy
	}
	if game.x >= game.width - width || game.x <= 0 {
		game.dx = - game.dx
	}
}

fn (game mut Game) run() {
	for {
		game.update_model()		
		glfw.post_empty_event() // Refresh
		time.sleep_ms(17)
	}
}
