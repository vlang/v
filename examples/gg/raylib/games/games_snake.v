module main

// raylib [games] classic - snake, ported to gg.
// Steer the snake with the arrow keys to eat the blue fruit and grow, without
// hitting the walls or its own tail. P pauses and ENTER restarts after a game
// over. PORT NOTE: raylib's IsKeyPressed maps to gg's filtered key_down events,
// and GetRandomValue to rand.intn; the grid and snake are plain gg primitives
// because the original uses no image or sound assets.

import gg
import rand

const screen_width = 800
const screen_height = 450

const snake_length = 256
const square_size = 31

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const dark_blue = gg.Color{0, 0, 139, 255}
const blue = gg.Color{0, 0, 255, 255}
const sky_blue = gg.Color{135, 206, 235, 255}

struct Segment {
mut:
	position gg.Vec2
	size     gg.Vec2
	speed    gg.Vec2
	color    gg.Color
}

struct Food {
mut:
	position gg.Vec2
	size     gg.Vec2
	active   bool
	color    gg.Color
}

struct App {
mut:
	ctx            &gg.Context = unsafe { nil }
	snake          []Segment
	snake_position []gg.Vec2
	fruit          Food
	offset         gg.Vec2
	counter_tail   int
	frames_counter int
	allow_move     bool
	game_over      bool
	paused         bool
}

fn random_food_position() gg.Vec2 {
	cx := rand.intn(screen_width / square_size) or { 0 }
	cy := rand.intn(screen_height / square_size) or { 0 }
	x := f32(cx * square_size) + f32(screen_width % square_size) / 2
	y := f32(cy * square_size) + f32(screen_height % square_size) / 2
	return gg.Vec2{x, y}
}

fn init_game(mut app &App) {
	app.frames_counter = 0
	app.game_over = false
	app.paused = false
	app.counter_tail = 1
	app.allow_move = false
	app.offset = gg.Vec2{f32(screen_width % square_size), f32(screen_height % square_size)}

	app.snake = []Segment{len: snake_length}
	for i in 0 .. snake_length {
		app.snake[i].position = gg.Vec2{app.offset.x / 2, app.offset.y / 2}
		app.snake[i].size = gg.Vec2{f32(square_size), f32(square_size)}
		app.snake[i].speed = gg.Vec2{f32(square_size), 0}
		app.snake[i].color = if i == 0 { dark_blue } else { blue }
	}
	app.snake_position = []gg.Vec2{len: snake_length, init: gg.Vec2{0, 0}}

	app.fruit.size = gg.Vec2{f32(square_size), f32(square_size)}
	app.fruit.color = sky_blue
	app.fruit.active = false
}

// A direction change is accepted only between movement steps and never reverses
// the current axis, mirroring the allowMove guard in the raylib original.
fn try_turn(mut app &App, dx f32, dy f32) {
	if app.game_over || app.paused || !app.allow_move {
		return
	}
	head := app.snake[0]
	if dx != 0 && head.speed.x != 0 {
		return
	}
	if dy != 0 && head.speed.y != 0 {
		return
	}
	app.snake[0].speed = gg.Vec2{dx, dy}
	app.allow_move = false
}

fn update(mut app &App) {
	// PORT NOTE: the original steps the snake every fifth frame at 60 FPS. gg's
	// dt is ignored here to keep that exact fixed, grid-aligned pacing.
	if app.game_over || app.paused {
		return
	}

	for i in 0 .. app.counter_tail {
		app.snake_position[i] = app.snake[i].position
	}

	if app.frames_counter % 5 == 0 {
		for i in 0 .. app.counter_tail {
			if i == 0 {
				app.snake[0].position.x += app.snake[0].speed.x
				app.snake[0].position.y += app.snake[0].speed.y
				app.allow_move = true
			} else {
				app.snake[i].position = app.snake_position[i - 1]
			}
		}
	}

	head := app.snake[0].position
	if head.x > f32(screen_width) - app.offset.x
		|| head.y > f32(screen_height) - app.offset.y
		|| head.x < 0 || head.y < 0 {
		app.game_over = true
	}

	for i in 1 .. app.counter_tail {
		if head.x == app.snake[i].position.x && head.y == app.snake[i].position.y {
			app.game_over = true
		}
	}

	if !app.fruit.active {
		app.fruit.active = true
		app.fruit.position = random_food_position()
		for i := 0; i < app.counter_tail; {
			for app.fruit.position.x == app.snake[i].position.x
				&& app.fruit.position.y == app.snake[i].position.y {
				app.fruit.position = random_food_position()
				i = 0
			}
			i++
		}
	}

	if head.x < app.fruit.position.x + app.fruit.size.x
		&& head.x + app.snake[0].size.x > app.fruit.position.x
		&& head.y < app.fruit.position.y + app.fruit.size.y
		&& head.y + app.snake[0].size.y > app.fruit.position.y {
		// PORT NOTE: the C original indexes a fixed snake[256] array and can run
		// past its end once the board is full; the length guard avoids that crash.
		if app.counter_tail < snake_length {
			app.snake[app.counter_tail].position = app.snake_position[app.counter_tail - 1]
			app.counter_tail++
		}
		app.fruit.active = false
	}

	app.frames_counter++
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	update(mut app)
}

fn centered_text(ctx &gg.Context, msg string, y int, size int, c gg.Color) {
	w := ctx.text_width_cfg(msg, size: size)
	ctx.draw_text(screen_width / 2 - w / 2, y, msg, size: size, color: c)
}

fn on_frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	if !app.game_over {
		for i in 0 .. screen_width / square_size + 1 {
			x := f32(square_size * i) + app.offset.x / 2
			ctx.draw_line(x, app.offset.y / 2, x, f32(screen_height) - app.offset.y / 2, light_gray)
		}
		for i in 0 .. screen_height / square_size + 1 {
			y := f32(square_size * i) + app.offset.y / 2
			ctx.draw_line(app.offset.x / 2, y, f32(screen_width) - app.offset.x / 2, y, light_gray)
		}

		for i in 0 .. app.counter_tail {
			s := app.snake[i]
			ctx.draw_rect_filled(s.position.x, s.position.y, s.size.x, s.size.y, s.color)
		}
		ctx.draw_rect_filled(app.fruit.position.x, app.fruit.position.y,
			app.fruit.size.x, app.fruit.size.y, app.fruit.color)

		if app.paused {
			centered_text(ctx, 'GAME PAUSED', screen_height / 2 - 40, 40, gray)
		}
	} else {
		centered_text(ctx, 'PRESS [ENTER] TO PLAY AGAIN', screen_height / 2 - 50, 20, gray)
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && !e.key_repeat {
		match e.key_code {
			.p {
				if !app.game_over {
					app.paused = !app.paused
				}
			}
			.enter {
				if app.game_over {
					init_game(mut app)
				}
			}
			.right { try_turn(mut app, f32(square_size), 0) }
			.left { try_turn(mut app, -f32(square_size), 0) }
			.up { try_turn(mut app, 0, -f32(square_size)) }
			.down { try_turn(mut app, 0, f32(square_size)) }
			else {}
		}
	}
}

fn main() {
	mut app := &App{}
	init_game(mut app)
	app.ctx = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [games] classic - snake'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
