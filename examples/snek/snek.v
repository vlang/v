import gg
import gx
import os
import rand
import time
import math.vec { Vec2 }

// constants
const font = $embed_file('../assets/fonts/RobotoMono-Regular.ttf')
const top_height = 100
const canvas_size = 700
const game_size = 17
const tile_size = canvas_size / game_size
const tick_rate_ms = 100
const high_score_file_path = os.join_path(os.cache_dir(), 'v', 'examples', 'snek')

// types
type HighScore = int
type Vec = Vec2[int]

fn (mut h HighScore) save() {
	os.mkdir_all(os.dir(high_score_file_path)) or { return }
	os.write_file(high_score_file_path, (*h).str()) or { return }
}

fn (mut h HighScore) load() {
	h = (os.read_file(high_score_file_path) or { '' }).int()
}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	score     int
	best      HighScore
	snake     []Vec
	dir       Vec
	dir_queue []Vec
	food      Vec
	last_tick i64
}

// utility
fn (mut app App) reset_game() {
	app.score = 0
	app.snake = [Vec{3, 8}, Vec{2, 8}, Vec{1, 8}, Vec{0, 8}]
	app.dir = Vec{1, 0}
	app.dir_queue = []
	app.food = Vec{10, 8}
	app.last_tick = time.ticks()
}

fn (mut app App) move_food() {
	for {
		x := rand.intn(game_size) or { 0 }
		y := rand.intn(game_size) or { 0 }
		app.food = Vec{x, y}
		if app.food !in app.snake {
			return
		}
	}
}

fn on_frame(mut app App) {
	// check if snake bit itself
	if app.snake[0] in app.snake[1..] {
		app.reset_game()
	}

	// check if snake hit a wall
	if app.snake[0].x < 0 || app.snake[0].x >= game_size || app.snake[0].y < 0
		|| app.snake[0].y >= game_size {
		app.reset_game()
	}
	progress := f32_min(1, f32(time.ticks() - app.last_tick) / f32(tick_rate_ms))

	// draw everything:
	app.gg.begin()
	// draw food
	app.gg.draw_rect_filled(tile_size * app.food.x, tile_size * app.food.y + top_height,
		tile_size, tile_size, gx.red)

	// draw snake
	for pos in app.snake[..app.snake.len - 1] {
		app.gg.draw_rect_filled(tile_size * pos.x, tile_size * pos.y + top_height, tile_size,
			tile_size, gx.blue)
	}

	// draw partial head
	head := app.snake[0]
	app.gg.draw_rect_filled(tile_size * (head.x + app.dir.x * progress), tile_size * (head.y +
		app.dir.y * progress) + top_height, tile_size, tile_size, gx.blue)

	// draw partial tail
	tail := app.snake.last()
	tail_dir := app.snake[app.snake.len - 2] - tail
	app.gg.draw_rect_filled(tile_size * (tail.x + tail_dir.x * progress), tile_size * (tail.y +
		tail_dir.y * progress) + top_height, tile_size, tile_size, gx.blue)

	// draw score bar
	app.gg.draw_rect_filled(0, 0, canvas_size, top_height, gx.black)
	app.gg.draw_text(150, top_height / 2, 'Score: ${app.score}', gx.TextCfg{
		color:          gx.white
		align:          .center
		vertical_align: .middle
		size:           65
	})
	app.gg.draw_text(canvas_size - 150, top_height / 2, 'Best: ${app.best}', gx.TextCfg{
		color:          gx.white
		align:          .center
		vertical_align: .middle
		size:           65
	})

	if progress == 1 {
		// "snake" along
		mut prev := app.snake[0]
		app.snake[0] = app.snake[0] + app.dir
		for i in 1 .. app.snake.len {
			app.snake[i], prev = prev, app.snake[i]
		}

		// add tail segment if food has been eaten
		if app.snake[0] == app.food {
			app.score++
			if app.score > app.best {
				app.best = app.score
				app.best.save()
			}
			app.snake << app.snake.last() + app.snake.last() - app.snake[app.snake.len - 2]
			app.move_food()
		}

		if app.dir_queue.len > 0 {
			dir := app.dir_queue.pop()
			if dir.x != -app.dir.x || dir.y != -app.dir.y {
				app.dir = dir
			}
		}

		app.last_tick = time.ticks()
	}

	app.gg.end()
}

// events
fn on_keydown(key gg.KeyCode, mod gg.Modifier, mut app App) {
	app.dir_queue << match key {
		.w, .up {
			Vec{0, -1}
		}
		.s, .down {
			Vec{0, 1}
		}
		.a, .left {
			Vec{-1, 0}
		}
		.d, .right {
			Vec{1, 0}
		}
		else {
			return
		}
	}
}

mut app := App{}
app.reset_game()
app.best.load()

mut font_copy := font
app.gg = gg.new_context(
	bg_color:          gx.white
	frame_fn:          on_frame
	keydown_fn:        on_keydown
	user_data:         &app
	width:             canvas_size
	height:            top_height + canvas_size
	window_title:      'snek'
	font_bytes_normal: unsafe { font_copy.data().vbytes(font_copy.len) }
)
app.gg.run()
