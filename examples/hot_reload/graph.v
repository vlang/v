module main

import gx
import gg
import time
import math

const (
	size  = 700
	scale = 50.0
)

struct Context {
mut:
	gg &gg.Context
}

fn main() {
	mut context := &Context{
		gg: 0
	}
	context.gg = gg.new_context(
		width: size
		height: size
		font_size: 20
		use_ortho: true
		user_data: context
		window_title: 'Graph builder'
		create_window: true
		frame_fn: frame
		resizable: true
		bg_color: gx.white
		font_path: gg.system_font_path()
	)
	context.gg.run()
}

fn frame(mut ctx Context) {
	ctx.gg.begin()
	ctx.draw()
	ctx.gg.end()
}

[live]
fn (ctx &Context) draw() {
	s := gg.window_size()
	mut w := s.width
	mut h := s.height
	if gg.high_dpi() {
		w /= 2
		h /= 2
	}
	ctx.gg.draw_line(0, h / 2, w, h / 2, gx.gray) // x axis
	ctx.gg.draw_line(w / 2, 0, w / 2, h, gx.gray) // y axis
	atime := f64(time.ticks() / 10)
	stime := math.sin(2.0 * math.pi * f64(time.ticks() % 6000) / 6000)
	mut y := 0.0
	blue := gx.Color{
		r: 100
		g: 100
		b: 200
	}
	red := gx.Color{
		r: 200
		g: 100
		b: 100
	}
	y = 1.0
	max := f32(w) / (2 * scale)
	min := -max
	for x := min; x <= max; x += 0.01 {
		// y = x*x + 2
		// y = x * x + stime * stime
		// y = stime
		// y = stime * h
		y = stime * 1.0 * math.sin(x + stime + atime / 32) * ((h / 256) + x)
		// y = (stime * x) * x + stime
		// y = (x + 3) * (x + 3) / stime + stime*2.5
		// y = math.sqrt(30.0 - x * x) * stime
		// y -= (stime-0.5) + stime
		// ctx.gg.draw_rect(f32((w/2) + x * scale), f32((h/2) - y * scale), 2, 2, blue)
		ctx.gg.draw_rect(f32((w / 2) + x * scale), f32((h / 2) - y * scale), 2, (f32(y) * scale),
			blue)
		ctx.gg.draw_rect(f32((w / 2) + x * scale), f32((h / 2) + y * scale), 2, (f32(y) * scale) +
			32, red)
	}
}
