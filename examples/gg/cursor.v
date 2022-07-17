module main

import gg
import gx
import sokol.sapp

fn main() {
	gg.new_context(
		bg_color: gx.white
		window_title: 'Cursor'
		frame_fn: frame
		init_fn: init
	).run()
}

fn init(mut ctx gg.Context) {
	sapp.set_mouse_cursor(.ibeam)
}

fn frame(mut ctx gg.Context) {
	ctx.begin()
	ctx.draw_text_def(10, 25, 'Your cursor should be an I shaped beam.')
	ctx.end()
}
