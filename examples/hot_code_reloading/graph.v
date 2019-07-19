module main

import gx
import gl
import gg
import glfw
	
const (
	WIDTH  = 1000
	HEIGHT = 1000
	SCALE  = 50
)

struct Context {
	mut:
	gg *gg.GG
}

fn main() {
	glfw.init()
	mut ctx := &Context{gg: 0}
	window := glfw.create_window(glfw.WinCfg{
		title: 'graph builder'
		width: 1000
		height: 1000
		ptr: ctx
	})
	window.make_context_current()
	gg.init()
	ctx.gg = gg.new_context(gg.Cfg {
		 width: 1000
		 height: 1000
		 use_ortho: true 
	})
	for {
		gl.clear()
		gl.clear_color(255, 255, 255, 255)
		ctx.draw()
		window.swap_buffers()
		glfw.wait_events()
	}
}

fn (ctx mut Context) draw() {
	// x axis
	ctx.gg.draw_line(0, HEIGHT / 2, WIDTH, HEIGHT / 2)
	// y axis
	ctx.gg.draw_line(WIDTH / 2, 0, WIDTH / 2, HEIGHT)
	mut prev_x := f64(0)
	mut prev_y := f64(0)
	center := f64(WIDTH / 2)
	for x := f64(- 10); x <= f64(10); x += 0.01 {
		y := x * x * f64(SCALE)
		// gx.draw_line_c(center + prev_x, center+prev_y, center + int(x*float(10)), center+y, gx.BLACK)
		ctx.gg.draw_rect(int(center) + int(x * f64(SCALE)), int(center - y), 1, 1, gx.Black)
		// gx.draw_rect_f(center + (x * f64(SCALE)), center - y, 1, 1, gx.BLACK)
		prev_x = x
		prev_y = y
	}
}
