module main

import gx
import gg
import time 
import glfw 
	
const (
	WIDTH  = 1000
	HEIGHT = 1000
	SCALE  = 50
)

struct Context {
	gg *gg.GG
}

fn main() {
	glfw.init()
	ctx:= &Context{ 
		gg: gg.new_context(gg.Cfg {
			width: WIDTH 
			height: HEIGHT 
			use_ortho: true 
			create_window: true 
			window_title: 'graph builder' 
			window_user_ptr: ctx 
		})
	} 
	for { 
		gg.clear(gx.White) 
		ctx.draw()
		ctx.gg.render() 
	}
}

[live] 
fn (ctx & Context) draw() {
	// x axis
	ctx.gg.draw_line(0, HEIGHT / 2, WIDTH, HEIGHT / 2)
	// y axis
	ctx.gg.draw_line(WIDTH / 2, 0, WIDTH / 2, HEIGHT)
	mut prev_x := f64(0)
	mut prev_y := f64(0)
	center := f64(WIDTH / 2)
	for x := f64(- 10); x <= f64(10); x += 0.01 {
		//y := (x * x - 2) * f64(SCALE)
		y := (1.0 / x) * f64(SCALE)
		//ctx.gg.draw_line(int(center + prev_x), int(center+prev_y), 
			//int(center + x*f64(10)), int(center+y))
		ctx.gg.draw_rect(int(center) + int(x * f64(SCALE)), int(center - y), 2, 1, gx.Black)
		// gx.draw_rect_f(center + (x * f64(SCALE)), center - y, 1, 1, gx.BLACK)
		prev_x = x
		prev_y = y
	}
}
