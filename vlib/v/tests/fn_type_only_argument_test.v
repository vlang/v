module main

import gg
import gx

struct Game {
	update fn (mut gg.Context) = fn (mut gg gg.Context) {}
	draw   fn (mut gg.Context) = fn (mut gg gg.Context) {}
mut:
	gg gg.Context
}

fn test_fn_type_only_argument() {
	mut game := Game{}
	game.gg = gg.new_context(
		bg_color: gx.rgb(230, 230, 240)
		frame_fn: frame
	)
	assert true
}

fn frame(mut game Game) {
	game.gg.begin()
	game.gg.draw_triangle_filled(450, 142, 530, 280, 370, 280, gx.red)
	game.update(mut game.gg)
	game.draw(mut game.gg)
	game.gg.end()
}
