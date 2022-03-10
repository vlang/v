module main

import time

struct Game {
	update fn (mut time.Time) = fn (mut time time.Time) {}
	draw   fn (mut time.Time) = fn (mut time time.Time) {}
mut:
	time time.Time
}

fn test_fn_type_only_argument() {
	mut game := Game{}
	game.time = time.Time{}
	assert true
}
