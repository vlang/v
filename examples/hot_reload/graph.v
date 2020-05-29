module main

import gx
import gg
import time
import glfw
import math

const (
	size  = 700
	scale  = 50.0
	pi = math.pi
)

struct Context {
	gg &gg.GG
}

fn main() {
	glfw.init_glfw()
	gconfig := gg.Cfg {
			width: size
			height: size
			use_ortho: true
			create_window: true
			window_title: 'Graph builder'
			always_on_top: true
	}
	ctx := &Context{ gg: gg.new_context(gconfig) }
	ctx.gg.window.set_user_ptr( ctx )
	go update() // update the scene in the background in case the window isn't focused
	for {
		if ctx.gg.window.should_close() {
			break
		}
		gg.clear(gx.white)
		ctx.draw()
		ctx.gg.render()
	}
}

[live]
fn (ctx &Context) draw() {
	center := f64(size / 2)
	ctx.gg.draw_line(0, center, size, center, gx.gray) // x axis
	ctx.gg.draw_line(center, 0, center, size, gx.gray) // y axis
	atime := f64( time.ticks() / 10 )
	stime := math.sin( 2.0 * pi * f64( time.ticks() % 6000 ) / 6000 )
	mut y := 0.0
	y = 1.0
	for x := -10.0; x <= 10.0; x += 0.02 {
		//y = x*x + 2
		y = x*x + stime*stime
		//y = stime
		//y = stime * x
		y = stime*1.0*math.sin(x + stime+atime/50) * x
		//y = (stime * x) * x + stime
		//y = (x + 3) * (x + 3) / stime + stime*2.5
		//y = math.sqrt(30.0 - x * x) * stime
		//y -= (stime-0.5) + stime
		ctx.gg.draw_rect(center + x * scale, center - y * scale, 1, 1, gx.blue)
		ctx.gg.draw_rect(center + x * scale, center + y * scale, 1, 1, gx.red)
	}
}

fn update() {
	for {
		gg.post_empty_event()
		time.sleep_ms(16) // 60 fps
	}
}
