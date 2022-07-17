module main

import gg
import gx
import sokol.sapp

fn main() {
	mut context := gg.new_context(
		bg_color: gx.rgb(255, 255, 255)
		width: 600
		height: 400
		window_title: 'Cursor'
		frame_fn: frame
		init_fn: init
	)
	context.run()
}

fn init(mut ctx gg.Context) {
	sapp.set_mouse_cursor(sapp.MouseCursor.text)
}

fn frame(mut ctx gg.Context) {
	ctx.begin()
	sapp.set_mouse_cursor(sapp.MouseCursor.text)
	ctx.draw_convex_poly([f32(100.0), 100.0, 200.0, 100.0, 300.0, 200.0, 200.0, 300.0, 100.0, 300.0],
		gx.blue)
	ctx.draw_poly_empty([f32(50.0), 50.0, 70.0, 60.0, 90.0, 80.0, 70.0, 110.0], gx.black)
	ctx.draw_triangle_filled(450, 142, 530, 280, 370, 280, gx.red)
	ctx.end()
}
