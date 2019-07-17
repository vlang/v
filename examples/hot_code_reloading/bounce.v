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
	gg       *gg.GG
	x        int
	y        int
	dy       int
	dx       int
	height   int
	width    int
	main_wnd *glfw.Window
	draw_fn  voidptr
}

fn main() {
	glfw.init()
	width := 600
	height := 300
	mut game := &Game{
		gg: 0 
		dx: 2 
		dy: 2 
		height: height 
		width: width 
	}
	mut window := glfw.create_window(glfw.WinCfg {
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
	gg.init() 
	game.gg = gg.new_context(gg.Cfg {
		width: width
		height: height
		font_size: 20
		use_ortho: true
	})
	println('Starting the game loop...')
	go game.run()
	for {
		gl.clear()
		gl.clear_color(255, 255, 255, 255)
		game.draw()
		window.swap_buffers()
		glfw.wait_events()
	}
}

const (
	W = 50
)

[live]
fn (game &Game) draw() {
	game.gg.draw_rect(game.x, game.y, W, W, gx.rgb(255, 0, 0)) 
}

fn (game mut Game) run() {
	for {
		game.x += game.dx
		game.y += game.dy
		if game.y >= game.height - W || game.y <= 0 {
			game.dy = - game.dy
		}
		if game.x >= game.width - W || game.x <= 0 {
			game.dx = - game.dx
		}
		// Refresh
		time.sleep_ms(17)
		glfw.post_empty_event()
	}
}


