import time

struct Game {
	update fn (mut time.Time) = fn (mut time_ time.Time) {}
	draw   fn (mut time.Time) = fn (mut time_ time.Time) {}
mut:
	time time.Time
}

fn test_fn_type_only_argument() {
	mut game := Game{}
	game.time = time.Time{}
	assert true
}
