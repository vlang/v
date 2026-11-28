module main

// raylib [games] classic - arkanoid, ported to gg.
// Bounce the ball with the paddle to clear all five rows of bricks before
// losing it five times. LEFT/RIGHT move the paddle, SPACE launches the ball,
// P pauses and ENTER restarts after a game over.
// PORT NOTE: raylib's IsKeyPressed/IsKeyDown helpers map to gg's filtered
// key_down events and ctx.pressed_keys; no image or sound assets are used by
// the original, so every visual is a plain gg primitive.

import gg

const screen_width = 800
const screen_height = 450

const player_max_life = 5
const lines_of_bricks = 5
const bricks_per_line = 20
const ball_radius = 7

const ray_white = gg.Color{245, 245, 245, 255}
const black = gg.Color{0, 0, 0, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{190, 33, 55, 255}

struct Player {
mut:
	position gg.Vec2
	size     gg.Vec2
	life     int
}

struct Ball {
mut:
	position gg.Vec2
	speed    gg.Vec2
	radius   f32
	active   bool
}

struct Brick {
mut:
	position gg.Vec2
	active   bool
}

struct App {
mut:
	ctx         &gg.Context = unsafe { nil }
	player      Player
	ball        Ball
	bricks      []Brick
	brick_size  gg.Vec2
	launch_ball bool
	game_over   bool
	paused      bool
}

fn brick_index(i int, j int) int {
	return i * bricks_per_line + j
}

fn init_game(mut app &App) {
	app.bricks = []Brick{len: lines_of_bricks * bricks_per_line}
	app.brick_size = gg.Vec2{f32(screen_width) / bricks_per_line, 40}

	app.player.position = gg.Vec2{f32(screen_width) / 2, f32(screen_height * 7 / 8)}
	app.player.size = gg.Vec2{f32(screen_width) / 10, 20}
	app.player.life = player_max_life

	app.ball.radius = f32(ball_radius)
	app.ball.position = gg.Vec2{app.player.position.x, app.player.position.y - app.player.size.y / 2 - app.ball.radius}
	app.ball.speed = gg.Vec2{0, 0}
	app.ball.active = false

	initial_down_position := f32(50)
	for i in 0 .. lines_of_bricks {
		for j in 0 .. bricks_per_line {
			b := brick_index(i, j)
			app.bricks[b].position = gg.Vec2{f32(j) * app.brick_size.x + app.brick_size.x / 2, f32(i) * app.brick_size.y + initial_down_position}
			app.bricks[b].active = true
		}
	}

	app.game_over = false
	app.paused = false
	app.launch_ball = false
}

fn absf(x f32) f32 {
	return if x < 0 { -x } else { x }
}

// PORT NOTE: gg has no CheckCollisionCircleRec(); use the same closest-point
// circle/rectangle overlap test that raylib performs internally.
fn circle_hits_rect(c gg.Vec2, radius f32, rx f32, ry f32, rw f32, rh f32) bool {
	closest_x := if c.x < rx {
		rx
	} else if c.x > rx + rw {
		rx + rw
	} else {
		c.x
	}
	closest_y := if c.y < ry {
		ry
	} else if c.y > ry + rh {
		ry + rh
	} else {
		c.y
	}
	dx := c.x - closest_x
	dy := c.y - closest_y
	return dx * dx + dy * dy <= radius * radius
}

fn update(mut app &App) {
	ctx := app.ctx
	// PORT NOTE: the original advances at a fixed 5 px per 60 FPS frame, so the
	// frame-rate independent dt from gg's on_update is deliberately ignored.
	if !app.game_over && !app.paused {
		if ctx.pressed_keys[gg.KeyCode.left] {
			app.player.position.x -= 5
		}
		if app.player.position.x - app.player.size.x / 2 <= 0 {
			app.player.position.x = app.player.size.x / 2
		}
		if ctx.pressed_keys[gg.KeyCode.right] {
			app.player.position.x += 5
		}
		if app.player.position.x + app.player.size.x / 2 >= f32(screen_width) {
			app.player.position.x = f32(screen_width) - app.player.size.x / 2
		}

		if app.launch_ball {
			app.launch_ball = false
			if !app.ball.active {
				app.ball.active = true
				app.ball.speed = gg.Vec2{0, -5}
			}
		}

		if app.ball.active {
			app.ball.position = app.ball.position + app.ball.speed
		} else {
			app.ball.position = gg.Vec2{app.player.position.x, app.player.position.y - app.player.size.y / 2 - app.ball.radius}
		}

		if app.ball.position.x + app.ball.radius >= f32(screen_width)
			|| app.ball.position.x - app.ball.radius <= 0 {
			app.ball.speed.x *= -1
		}
		if app.ball.position.y - app.ball.radius <= 0 {
			app.ball.speed.y *= -1
		}
		if app.ball.position.y + app.ball.radius >= f32(screen_height) {
			app.ball.speed = gg.Vec2{0, 0}
			app.ball.active = false
			app.player.life--
		}

		if app.ball.active && circle_hits_rect(app.ball.position, app.ball.radius,
			app.player.position.x - app.player.size.x / 2,
			app.player.position.y - app.player.size.y / 2,
			app.player.size.x, app.player.size.y) {
			if app.ball.speed.y > 0 {
				app.ball.speed.y *= -1
				app.ball.speed.x = (app.ball.position.x - app.player.position.x) /
					(app.player.size.x / 2) * 5
			}
		}

		// The extra hit margin is f32(ball_radius*2/3), matching the original's
		// integer arithmetic with radius 7.
		extra := f32(ball_radius * 2 / 3)
		for i in 0 .. lines_of_bricks {
			for j in 0 .. bricks_per_line {
				idx := brick_index(i, j)
				if !app.bricks[idx].active {
					continue
				}
				bp := app.bricks[idx].position
				if (app.ball.position.y - app.ball.radius <= bp.y + app.brick_size.y / 2)
					&& (app.ball.position.y - app.ball.radius > bp.y + app.brick_size.y / 2 + app.ball.speed.y)
					&& absf(app.ball.position.x - bp.x) < app.brick_size.x / 2 + extra
					&& app.ball.speed.y < 0 {
					app.bricks[idx].active = false
					app.ball.speed.y *= -1
				} else if (app.ball.position.y + app.ball.radius >= bp.y - app.brick_size.y / 2)
					&& (app.ball.position.y + app.ball.radius < bp.y - app.brick_size.y / 2 + app.ball.speed.y)
					&& absf(app.ball.position.x - bp.x) < app.brick_size.x / 2 + extra
					&& app.ball.speed.y > 0 {
					app.bricks[idx].active = false
					app.ball.speed.y *= -1
				} else if (app.ball.position.x + app.ball.radius >= bp.x - app.brick_size.x / 2)
					&& (app.ball.position.x + app.ball.radius < bp.x - app.brick_size.x / 2 + app.ball.speed.x)
					&& absf(app.ball.position.y - bp.y) < app.brick_size.y / 2 + extra
					&& app.ball.speed.x > 0 {
					app.bricks[idx].active = false
					app.ball.speed.x *= -1
				} else if (app.ball.position.x - app.ball.radius <= bp.x + app.brick_size.x / 2)
					&& (app.ball.position.x - app.ball.radius > bp.x + app.brick_size.x / 2 + app.ball.speed.x)
					&& absf(app.ball.position.y - bp.y) < app.brick_size.y / 2 + extra
					&& app.ball.speed.x < 0 {
					app.bricks[idx].active = false
					app.ball.speed.x *= -1
				}
			}
		}

		if app.player.life <= 0 {
			app.game_over = true
		} else {
			app.game_over = true
			for i in 0 .. lines_of_bricks {
				for j in 0 .. bricks_per_line {
					if app.bricks[brick_index(i, j)].active {
						app.game_over = false
					}
				}
			}
		}
	}
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
		ctx.draw_rect_filled(app.player.position.x - app.player.size.x / 2,
			app.player.position.y - app.player.size.y / 2,
			app.player.size.x, app.player.size.y, black)

		for i in 0 .. app.player.life {
			ctx.draw_rect_filled(f32(20 + 40 * i), f32(screen_height - 30), 35, 10, light_gray)
		}

		ctx.draw_circle_filled(app.ball.position.x, app.ball.position.y, app.ball.radius, maroon)

		for i in 0 .. lines_of_bricks {
			for j in 0 .. bricks_per_line {
				b := app.bricks[brick_index(i, j)]
				if !b.active {
					continue
				}
				c := if (i + j) % 2 == 0 { gray } else { dark_gray }
				ctx.draw_rect_filled(b.position.x - app.brick_size.x / 2,
					b.position.y - app.brick_size.y / 2,
					app.brick_size.x, app.brick_size.y, c)
			}
		}

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
			.space {
				if !app.game_over && !app.paused && !app.ball.active {
					app.launch_ball = true
				}
			}
			.enter {
				if app.game_over {
					init_game(mut app)
				}
			}
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
		window_title: 'raylib [games] classic - arkanoid'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
