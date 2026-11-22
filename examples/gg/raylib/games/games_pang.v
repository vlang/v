module main

// raylib [games] classic - pang, ported to gg.
// A cannon sits at the bottom of the screen, slides left/right and fires a
// single vertical bolt. Big bouncing balls split into two medium balls when
// hit, medium balls split into two small balls, and small balls disappear.
// Clear every ball to win; touch any ball and the game is over.
// LEFT/RIGHT move, SPACE fires, P pauses, ENTER restarts.

// PORT NOTE: no external assets or audio are used by the original classic;
// every visual is drawn with gg primitives. The original's unused `lose` and
// `awake` flags are omitted.

import gg
import math
import rand

const screen_width = 800
const screen_height = 450

const player_base_size = f32(20)
const player_speed = f32(5)
const player_max_shoots = 1
const player_collider_radius = f32(12)

const max_big_balls = 2
const max_medium_balls = max_big_balls * 2
const max_small_balls = max_big_balls * 4
const points_count = 5

const balls_speed = f32(2)
const total_balls = max_big_balls + max_medium_balls + max_small_balls

const deg2rad = f32(0.01745329252)
const gravity = f32(0.25)
const ship_height = f32(10) / math.tanf(f32(20) * deg2rad)

const shoot_life = 120

const ray_white = gg.Color{245, 245, 245, 255}
const maroon = gg.Color{128, 0, 0, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const red = gg.Color{230, 41, 55, 255}
const blue = gg.Color{0, 121, 241, 255}
const faded_ball = gg.Color{200, 200, 200, 76}

struct Player {
mut:
	position gg.Vec2
	speed    gg.Vec2
	rotation f32
}

struct Shoot {
mut:
	position   gg.Vec2
	speed      gg.Vec2
	radius     f32
	life_spawn int
	active     bool
}

struct Ball {
mut:
	position gg.Vec2
	speed    gg.Vec2
	radius   f32
	points   int
	active   bool
}

struct Points {
mut:
	position gg.Vec2
	value    int
	alpha    f32
}

struct App {
mut:
	ctx               &gg.Context = unsafe { nil }
	player            Player
	shoots            []Shoot
	big_balls         []Ball
	medium_balls      []Ball
	small_balls       []Ball
	points            []Points
	frames_counter    int
	score             int
	meteors_destroyed int
	count_medium      int
	count_small       int
	line_position     gg.Vec2
	shoot_requested   bool
	game_over         bool
	paused            bool
	victory           bool
}

fn rand_range(lo int, hi int) int {
	return lo + (rand.intn(hi - lo + 1) or { 0 })
}

fn pad2(n int) string {
	s := n.str()
	if s.len < 2 {
		return '0${s}'
	}
	return s
}

fn circles_collide(ax f32, ay f32, ar f32, bx f32, by f32, br f32) bool {
	dx := ax - bx
	dy := ay - by
	r := ar + br
	return dx * dx + dy * dy <= r * r
}

fn add_floating_points(mut app &App, at gg.Vec2, value int) {
	for z in 0 .. points_count {
		if app.points[z].alpha == 0 {
			app.points[z].position = at
			app.points[z].value = value
			app.points[z].alpha = 1
			return
		}
	}
}

fn spawn_medium_balls(mut app &App, at gg.Vec2) {
	for j := 0; j < 2; j++ {
		i := app.count_medium
		app.medium_balls[i].position = at
		if app.count_medium % 2 == 0 {
			app.medium_balls[i].speed = gg.Vec2{-balls_speed, balls_speed}
		} else {
			app.medium_balls[i].speed = gg.Vec2{balls_speed, balls_speed}
		}
		app.medium_balls[i].active = true
		app.count_medium++
	}
}

fn spawn_small_balls(mut app &App, at gg.Vec2) {
	for j := 0; j < 2; j++ {
		i := app.count_small
		app.small_balls[i].position = at
		if app.count_small % 2 == 0 {
			app.small_balls[i].speed = gg.Vec2{-balls_speed, -balls_speed}
		} else {
			app.small_balls[i].speed = gg.Vec2{balls_speed, -balls_speed}
		}
		app.small_balls[i].active = true
		app.count_small++
	}
}

fn init_game(mut app &App) {
	app.frames_counter = 0
	app.game_over = false
	app.paused = false
	app.score = 0
	app.victory = false
	app.shoot_requested = false
	app.line_position = gg.Vec2{}
	app.meteors_destroyed = 0
	app.count_medium = 0
	app.count_small = 0

	app.player.position = gg.Vec2{f32(screen_width) / 2, f32(screen_height)}
	app.player.speed = gg.Vec2{player_speed, player_speed}
	app.player.rotation = 0

	app.shoots = []Shoot{len: player_max_shoots}
	for i in 0 .. player_max_shoots {
		app.shoots[i].radius = 2
	}

	app.big_balls = []Ball{len: max_big_balls}
	for i in 0 .. max_big_balls {
		mut vx := 0
		mut vy := 0
		for vx == 0 || vy == 0 {
			vx = rand_range(-int(balls_speed), int(balls_speed))
			vy = rand_range(-int(balls_speed), int(balls_speed))
		}
		app.big_balls[i].radius = 40
		app.big_balls[i].position = gg.Vec2{f32(rand_range(int(app.big_balls[i].radius), screen_width - int(app.big_balls[i].radius))), f32(rand_range(int(app.big_balls[i].radius), screen_height / 2))}
		app.big_balls[i].speed = gg.Vec2{f32(vx), f32(vy)}
		app.big_balls[i].points = 200
		app.big_balls[i].active = true
	}

	app.medium_balls = []Ball{len: max_medium_balls}
	for i in 0 .. max_medium_balls {
		app.medium_balls[i].radius = 20
		app.medium_balls[i].points = 100
		app.medium_balls[i].active = false
	}

	app.small_balls = []Ball{len: max_small_balls}
	for i in 0 .. max_small_balls {
		app.small_balls[i].radius = 10
		app.small_balls[i].points = 50
		app.small_balls[i].active = false
	}

	app.points = []Points{len: points_count}
	for i in 0 .. points_count {
		app.points[i].alpha = 0
	}
}

fn move_big_balls(mut app &App) {
	for i in 0 .. max_big_balls {
		mut b := app.big_balls[i]
		if !b.active {
			continue
		}
		b.position.x += b.speed.x
		b.position.y += b.speed.y
		if b.position.x + b.radius >= f32(screen_width) || b.position.x - b.radius <= 0 {
			b.speed.x *= -1
		}
		if b.position.y - b.radius <= 0 {
			b.speed.y *= -1.5
		}
		if b.position.y + b.radius >= f32(screen_height) {
			b.speed.y *= -1
			b.position.y = f32(screen_height) - b.radius
		}
		b.speed.y += gravity
		app.big_balls[i] = b
	}
}

fn move_small_balls(mut balls []Ball, extra_gravity f32) {
	for i in 0 .. balls.len {
		mut b := balls[i]
		if !b.active {
			continue
		}
		b.position.x += b.speed.x
		b.position.y += b.speed.y
		if b.position.x + b.radius >= f32(screen_width) || b.position.x - b.radius <= 0 {
			b.speed.x *= -1
		}
		if b.position.y - b.radius <= 0 {
			b.speed.y *= -1
		}
		if b.position.y + b.radius >= f32(screen_height) {
			b.speed.y *= -1
			b.position.y = f32(screen_height) - b.radius
		}
		b.speed.y += gravity + extra_gravity
		balls[i] = b
	}
}

fn update(mut app &App) {
	app.frames_counter++

	if !app.game_over && !app.victory {
		if !app.paused {
			// Player movement (IsKeyDown in the original).
			if app.ctx.pressed_keys[int(gg.KeyCode.left)] {
				app.player.position.x -= app.player.speed.x
			}
			if app.ctx.pressed_keys[int(gg.KeyCode.right)] {
				app.player.position.x += app.player.speed.x
			}
			if app.player.position.x + player_base_size / 2 > f32(screen_width) {
				app.player.position.x = f32(screen_width) - player_base_size / 2
			} else if app.player.position.x - player_base_size / 2 < 0 {
				app.player.position.x = player_base_size / 2
			}

			// Firing is a one-shot action delivered by the keyboard event.
			if app.shoot_requested {
				app.shoot_requested = false
				for i in 0 .. player_max_shoots {
					if !app.shoots[i].active {
						app.shoots[i].position = gg.Vec2{app.player.position.x, app.player.position.y - ship_height}
						app.shoots[i].speed.y = player_speed
						app.shoots[i].active = true
						app.line_position = app.player.position
						break
					}
				}
			}

			for i in 0 .. player_max_shoots {
				if app.shoots[i].active {
					app.shoots[i].life_spawn++
				}
			}
			for i in 0 .. player_max_shoots {
				if app.shoots[i].active {
					app.shoots[i].position.y -= app.shoots[i].speed.y
					p := app.shoots[i].position
					r := app.shoots[i].radius
					if p.x > f32(screen_width) + r || p.x < -r || p.y > f32(screen_height) + r || p.y < -r {
						app.shoots[i].active = false
						app.shoots[i].life_spawn = 0
					}
					if app.shoots[i].life_spawn >= shoot_life {
						app.shoots[i].position = gg.Vec2{}
						app.shoots[i].speed = gg.Vec2{}
						app.shoots[i].life_spawn = 0
						app.shoots[i].active = false
					}
				}
			}

			// Player collider sits halfway up the ship.
			cx := app.player.position.x
			cy := app.player.position.y - ship_height / 2
			for i in 0 .. max_big_balls {
				b := app.big_balls[i]
				if b.active && circles_collide(cx, cy, player_collider_radius, b.position.x, b.position.y, b.radius) {
					app.game_over = true
				}
			}
			for i in 0 .. max_medium_balls {
				b := app.medium_balls[i]
				if b.active && circles_collide(cx, cy, player_collider_radius, b.position.x, b.position.y, b.radius) {
					app.game_over = true
				}
			}
			for i in 0 .. max_small_balls {
				b := app.small_balls[i]
				if b.active && circles_collide(cx, cy, player_collider_radius, b.position.x, b.position.y, b.radius) {
					app.game_over = true
				}
			}

			// Ball movement and gravity.
			move_big_balls(mut app)
			move_small_balls(mut app.medium_balls, 0.12)
			move_small_balls(mut app.small_balls, 0.25)

			// Bolt vs balls: a hit is registered when the ball's horizontal span
			// crosses the bolt's firing column and its lower edge reaches the bolt.
			for i in 0 .. player_max_shoots {
				if !app.shoots[i].active {
					continue
				}
				bolt_y := app.shoots[i].position.y
				bolt_x := app.line_position.x
				for a in 0 .. max_big_balls {
					b := app.big_balls[a]
					if b.active && b.position.x - b.radius <= bolt_x && b.position.x + b.radius >= bolt_x
						&& b.position.y + b.radius >= bolt_y {
						app.shoots[i].active = false
						app.shoots[i].life_spawn = 0
						app.big_balls[a].active = false
						app.meteors_destroyed++
						app.score += b.points
						add_floating_points(mut app, b.position, b.points)
						spawn_medium_balls(mut app, b.position)
						break
					}
				}
				if !app.shoots[i].active {
					continue
				}
				for b in 0 .. max_medium_balls {
					ball := app.medium_balls[b]
					if ball.active && ball.position.x - ball.radius <= bolt_x
						&& ball.position.x + ball.radius >= bolt_x && ball.position.y + ball.radius >= bolt_y {
						app.shoots[i].active = false
						app.shoots[i].life_spawn = 0
						app.medium_balls[b].active = false
						app.meteors_destroyed++
						app.score += ball.points
						add_floating_points(mut app, ball.position, ball.points)
						spawn_small_balls(mut app, ball.position)
						break
					}
				}
				if !app.shoots[i].active {
					continue
				}
				for c in 0 .. max_small_balls {
					ball := app.small_balls[c]
					if ball.active && ball.position.x - ball.radius <= bolt_x
						&& ball.position.x + ball.radius >= bolt_x && ball.position.y + ball.radius >= bolt_y {
						app.shoots[i].active = false
						app.shoots[i].life_spawn = 0
						app.small_balls[c].active = false
						app.meteors_destroyed++
						app.score += ball.points
						add_floating_points(mut app, ball.position, ball.points)
						break
					}
				}
			}

			if app.meteors_destroyed == total_balls {
				app.victory = true
			}
		}
	}

	// Floating score labels rise and fade every frame, even after game over.
	for z in 0 .. points_count {
		if app.points[z].alpha > 0 {
			app.points[z].position.y -= 2
			app.points[z].alpha -= 0.02
		}
		if app.points[z].alpha < 0 {
			app.points[z].alpha = 0
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

fn draw_balls(ctx &gg.Context, balls []Ball, active_color gg.Color) {
	for i in 0 .. balls.len {
		b := balls[i]
		c := if b.active { active_color } else { faded_ball }
		ctx.draw_circle_filled(b.position.x, b.position.y, b.radius, c)
	}
}

fn on_frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	if !app.game_over {
		// Draw the player as an upright isosceles triangle (rotation stays 0).
		v1x := app.player.position.x
		v1y := app.player.position.y - ship_height
		v2x := app.player.position.x - player_base_size / 2
		v2y := app.player.position.y
		v3x := app.player.position.x + player_base_size / 2
		v3y := app.player.position.y
		ctx.draw_triangle_filled(v1x, v1y, v2x, v2y, v3x, v3y, maroon)

		draw_balls(ctx, app.big_balls, dark_gray)
		draw_balls(ctx, app.medium_balls, gray)
		draw_balls(ctx, app.small_balls, gray)

		for i in 0 .. player_max_shoots {
			s := app.shoots[i]
			if s.active {
				ctx.draw_line(app.line_position.x, app.line_position.y, s.position.x, s.position.y, red)
			}
		}

		for z in 0 .. points_count {
			p := app.points[z]
			if p.alpha > 0 {
				a := u8(p.alpha * 255)
				c := gg.Color{blue.r, blue.g, blue.b, a}
				ctx.draw_text(int(p.position.x), int(p.position.y), '+${pad2(p.value)}',
					size:  20
					color: c
				)
			}
		}

		ctx.draw_text(10, 10, 'SCORE: ${app.score}', size: 20, color: light_gray)

		if app.victory {
			centered_text(ctx, 'YOU WIN!', 100, 60, light_gray)
			centered_text(ctx, 'PRESS [ENTER] TO PLAY AGAIN', screen_height / 2 - 50, 20, light_gray)
		}
		if app.paused {
			centered_text(ctx, 'GAME PAUSED', screen_height / 2 - 40, 40, light_gray)
		}
	} else {
		centered_text(ctx, 'PRESS [ENTER] TO PLAY AGAIN', screen_height / 2 - 50, 20, light_gray)
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && !e.key_repeat {
		match e.key_code {
			.p {
				if !app.game_over && !app.victory {
					app.paused = !app.paused
				}
			}
			.space {
				if !app.game_over && !app.victory && !app.paused {
					app.shoot_requested = true
				}
			}
			.enter {
				if app.game_over || app.victory {
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
		window_title: 'raylib [games] classic - pang'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
