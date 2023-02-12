// Build this example with
// v -live bounce.v
module main

import gx
import gg
import time

struct Game {
mut:
	gg      &gg.Context = unsafe { nil }
	x       int
	y       int
	dy      int
	dx      int
	height  int
	width   int
	draw_fn voidptr
}

const (
	window_width  = 400
	window_height = 300
	width         = 50
)

fn main() {
	mut game := &Game{
		gg: 0
		dx: 2
		dy: 2
		height: window_height
		width: window_width
		draw_fn: 0
	}
	game.gg = gg.new_context(
		width: window_width
		height: window_height
		font_size: 20
		user_data: game
		window_title: 'Hot code reloading demo'
		create_window: true
		frame_fn: frame
		bg_color: gx.white
	)
	// window.onkeydown(key_down)
	println('Starting the game loop...')
	spawn game.run()
	game.gg.run()
}

// Try uncommenting or changing the lines inside the live functions.
// Guess what will happen:
[live]
fn frame(mut game Game) {
	game.gg.begin()
	game.gg.draw_text_def(10, 5, 'Modify examples/hot_reload/bounce.v to get instant updates')
	game.gg.draw_rect_filled(game.x, game.y, width, width, gx.blue)
	game.gg.draw_rect_filled(window_width - width - game.x + 10, 200 - game.y + width,
		width, width, gx.rgb(228, 10, 55))
	game.gg.draw_rect_filled(game.x - 25, 250 - game.y, width, width, gx.rgb(28, 240,
		55))
	game.gg.end()
}

[live]
fn (mut game Game) update_model() {
	speed := 2
	game.x += speed * game.dx
	game.y += speed * game.dy
	if game.y >= game.height - width || game.y <= 0 {
		game.dy = -game.dy
	}
	if game.x >= game.width - width || game.x <= 0 {
		game.dx = -game.dx
	}
}

fn (mut game Game) run() {
	for {
		game.update_model()
		time.sleep(16 * time.millisecond) // 60fps
	}
}
