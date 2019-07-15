// Build this example with
// v -live -sanitize bounce.v
module main

import gx
import gl
import gg
import glfw
import time

struct Game {
mut: 
	vg       *gg.GG
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
		vg: 0 
		dx: 3 
		dy: 3 
		height: height 
		width: width 
	}
	mut window := glfw.create_window(glfw.WinCfg {
		width: width,
		height: height,
		borderless: false,
		title: 'Hot code reloading demo'
		ptr: game 
	}) 
	//window.onkeydown(key_down)
	game.main_wnd = window 
	window.make_context_current()
	gg.init() 
	game.vg = gg.new_context(gg.Cfg {
		width: width
		height: height
		font_size: 20
		use_ortho: true
	})
	println('Starting game loop...')
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
fn (ctx &Game) draw() {
	ctx.vg.draw_rect(ctx.x, ctx.y, W, W, gx.rgb(0, 0, 255))
}

fn (ctx mut Game) run() {
	for {
		ctx.x += ctx.dx
		ctx.y += ctx.dy
		if ctx.y >= ctx.height - W || ctx.y <= 0 {
			ctx.dy = - ctx.dy
		}
		if ctx.x >= ctx.width - W || ctx.x <= 0 {
			ctx.dx = - ctx.dx
		}
		// Refresh
		time.sleep_ms(30)
		glfw.post_empty_event()
	}
}


