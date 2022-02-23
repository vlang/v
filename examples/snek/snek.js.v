import gg
import gx
// import sokol.sapp
import time
import rand

// constants
const (
	top_height   = 100
	canvas_size  = 700
	game_size    = 17
	tile_size    = canvas_size / game_size
	tick_rate_ms = 100
)

// types
struct Pos {
	x int
	y int
}

fn (a Pos) + (b Pos) Pos {
	return Pos{a.x + b.x, a.y + b.y}
}

fn (a Pos) - (b Pos) Pos {
	return Pos{a.x - b.x, a.y - b.y}
}

enum Direction {
	up
	down
	left
	right
}

struct App {
mut:
	gg         &gg.Context
	score      int
	snake      []Pos
	dir        Direction
	last_dir   Direction
	food       Pos
	start_time i64
	last_tick  i64
}

// utility
fn (mut app App) reset_game() {
	app.score = 0
	app.snake = [
		Pos{3, 8},
		Pos{2, 8},
		Pos{1, 8},
		Pos{0, 8},
	]
	app.dir = .right
	app.last_dir = app.dir
	app.food = Pos{10, 8}
	app.start_time = time.ticks()
	app.last_tick = time.ticks()
}

fn (mut app App) move_food() {
	for {
		x := rand.intn(game_size) or { 0 }
		y := rand.intn(game_size) or { 0 }
		app.food = Pos{x, y}

		if app.food !in app.snake {
			return
		}
	}
}

// events
fn on_keydown(key gg.KeyCode, mod gg.Modifier, mut app App) {
	match key {
		.w, .up {
			if app.last_dir != .down {
				app.dir = .up
			}
		}
		.s, .down {
			if app.last_dir != .up {
				app.dir = .down
			}
		}
		.a, .left {
			if app.last_dir != .right {
				app.dir = .left
			}
		}
		.d, .right {
			if app.last_dir != .left {
				app.dir = .right
			}
		}
		else {}
	}
}

fn on_frame(mut app App) {
	app.gg.begin()

	now := time.ticks()

	if now - app.last_tick >= tick_rate_ms {
		app.last_tick = now

		// finding delta direction
		delta_dir := match app.dir {
			.up { Pos{0, -1} }
			.down { Pos{0, 1} }
			.left { Pos{-1, 0} }
			.right { Pos{1, 0} }
		}

		// "snaking" along
		mut prev := app.snake[0]
		app.snake[0] = app.snake[0] + delta_dir

		for i in 1 .. app.snake.len {
			tmp := app.snake[i]
			app.snake[i] = prev
			prev = tmp
		}

		// adding last segment
		if app.snake[0] == app.food {
			app.move_food()
			app.score++
			/*
			if app.score > app.best {
				app.best = app.score
				app.best.save()
			}*/
			app.snake << app.snake.last() + app.snake.last() - app.snake[app.snake.len - 2]
		}

		app.last_dir = app.dir
	}
	// drawing snake
	for pos in app.snake {
		app.gg.draw_rect(tile_size * pos.x, tile_size * pos.y + top_height, tile_size,
			tile_size, gx.blue)
	}

	// drawing food
	app.gg.draw_rect(tile_size * app.food.x, tile_size * app.food.y + top_height, tile_size,
		tile_size, gx.red)

	// drawing top
	app.gg.draw_rect(0, 0, canvas_size, top_height, gx.black)
	app.gg.draw_text(350, top_height / 2, 'Score: $app.score', gx.TextCfg{
		color: gx.white
		align: .center
		vertical_align: .middle
		size: 80
	})

	// checking if snake bit itself
	if app.snake[0] in app.snake[1..] {
		app.reset_game()
	}
	// checking if snake hit a wall
	if app.snake[0].x < 0 || app.snake[0].x >= game_size || app.snake[0].y < 0
		|| app.snake[0].y >= game_size {
		app.reset_game()
	}

	app.gg.end()
}

// setup
fn main() {
	mut app := App{
		gg: &gg.Context{}
	}
	app.reset_game()

	app.gg = gg.new_context(
		bg_color: gx.white
		frame_fn: on_frame
		keydown_fn: on_keydown
		user_data: &app
		width: canvas_size
		height: top_height + canvas_size
		create_window: true
		resizable: false
		window_title: 'snek'
		canvas: 'canvas'
	)

	app.gg.run()
}
