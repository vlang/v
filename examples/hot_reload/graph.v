module main

import gx
import gg
import time
import math

const (
	size  = 700
	scale = 50.0
	pi    = math.pi
)

struct Context {
mut:
	gg &gg.Context
}

fn main() {
	mut context := &Context{
		gg: 0
	}
	context.gg = gg.new_context({
		width: size
		height: size
		font_size: 20
		use_ortho: true
		user_data: context
		window_title: 'Graph builder'
		create_window: true
		frame_fn: frame
		bg_color: gx.white
		font_path: gg.system_font_path()
	})
	context.gg.run()
}

fn frame(mut ctx Context) {
	ctx.gg.begin()
	ctx.draw()
	ctx.gg.end()
}

[live]
fn (ctx &Context) draw() {
	center := f32(size / 2)
	ctx.gg.draw_line(0, center, size, center, gx.gray) // x axis
	ctx.gg.draw_line(center, 0, center, size, gx.gray) // y axis
	atime := f64(time.ticks() / 10)
	stime := math.sin(2.0 * pi * f64(time.ticks() % 6000) / 6000)
	mut y := 0.0
	y = 1.0
	for x := -10.0; x <= 10.0; x += 0.02 {
		// y = x*x + 2
		y = x * x + stime * stime
		// y = stime
		// y = stime * x
		y = stime * 1.0 * math.sin(x + stime + atime / 50) * x
		// y = (stime * x) * x + stime
		// y = (x + 3) * (x + 3) / stime + stime*2.5
		// y = math.sqrt(30.0 - x * x) * stime
		// y -= (stime-0.5) + stime
		ctx.gg.draw_rect(f32(center + x * scale), f32(center - y * scale), 1, 1, gx.blue)
		ctx.gg.draw_rect(f32(center + x * scale), f32(center + y * scale), 1, 1, gx.red)
	}
}
