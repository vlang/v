module main

import gg
import gx
import os

fn main() {
	scount := os.args[1] or { '2' }.int()
	println('> sample count: ${scount}')
	mut ctx := gg.new_context(
		bg_color:     gx.white
		window_title: 'sample_count: ${scount}'
		width:        320
		height:       240
		sample_count: scount
		frame_fn:     fn (mut ctx gg.Context) {
			ctx.begin()
			ctx.draw_rounded_rect_empty(110, 70, 100, 100, 10, gx.blue)
			ctx.draw_circle_empty(160, 120, 100, gx.red)
			ctx.draw_triangle_empty(160, 93, 186, 138, 132, 138, gx.green)
			ctx.draw_rect_filled(159, 119, 2, 2, gx.black)
			ctx.end()
		}
	)
	ctx.run()
}
