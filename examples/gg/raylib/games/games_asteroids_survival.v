module main

// raylib [games] classic - asteroids survival, ported to gg.
// Pilot a small ship through a drifting field of meteors. Rotate with LEFT and
// RIGHT, thrust with UP, brake with DOWN. Both the ship and the meteors wrap
// around the screen edges. Touching any meteor ends the run; survive as long as
// you can. P pauses, ENTER restarts.
// PORT NOTE: the original defines PLAYER_MAX_SHOOTS but never shoots, so it is
// omitted here. Fade(LIGHTGRAY, 0.3) is approximated by a solid pale gray; the
// ship and meteors are drawn with gg triangle/circle primitives.

import gg
import math
import rand

const screen_width = 800
const screen_height = 450

const player_base_size = f32(20)
const player_speed = f32(6)
const meteors_speed = 2
const max_medium_meteors = 8
const max_small_meteors = 16

const deg2rad = f32(0.017453292519943295)

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const green = gg.Color{0, 255, 0, 255}
const yellow = gg.Color{255, 255, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}
const black = gg.Color{0, 0, 0, 255}
// PORT NOTE: approximation of raylib's Fade(LIGHTGRAY, 0.3f).
const faded_light_gray = gg.Color{232, 232, 232, 255}

struct Player {
mut:
	position     gg.Vec2
	speed        gg.Vec2
	acceleration f32
	rotation     f32
	collider_x   f32
	collider_y   f32
	collider_r   f32
	color        gg.Color
}

struct Meteor {
mut:
	position gg.Vec2
	speed    gg.Vec2
	radius   f32
	active   bool
	color    gg.Color
}

struct App {
mut:
	ctx            &gg.Context = unsafe { nil }
	player         Player
	medium_meteors []Meteor
	small_meteors  []Meteor
	frames_counter int
	game_over      bool
	paused         bool
	ship_height    f32
}

fn rand_range(lo int, hi int) int {
	return lo + (rand.intn(hi - lo + 1) or { 0 })
}

// rand_meteor_pos picks a spawn outside the clear zone around the player.
fn rand_meteor_pos() gg.Vec2 {
	for {
		x := rand_range(0, screen_width)
		if x > screen_width / 2 - 150 && x < screen_width / 2 + 150 {
			continue
		}
		for {
			y := rand_range(0, screen_height)
			if y > screen_height / 2 - 150 && y < screen_height / 2 + 150 {
				continue
			}
			return gg.Vec2{f32(x), f32(y)}
		}
	}
}

// rand_meteor_speed returns a nonzero drift vector within [-METEORS_SPEED, +].
fn rand_meteor_speed() gg.Vec2 {
	for {
		vx := rand_range(-meteors_speed, meteors_speed)
		vy := rand_range(-meteors_speed, meteors_speed)
		if vx == 0 && vy == 0 {
			continue
		}
		return gg.Vec2{f32(vx), f32(vy)}
	}
}

fn circles_collide(c1 gg.Vec2, r1 f32, c2 gg.Vec2, r2 f32) bool {
	dx := c1.x - c2.x
	dy := c1.y - c2.y
	rr := r1 + r2
	return dx * dx + dy * dy <= rr * rr
}

fn update_collider(mut app &App) {
	r := app.player.rotation * deg2rad
	app.player.collider_x = app.player.position.x + math.sinf(r) * (app.ship_height / f32(2.5))
	app.player.collider_y = app.player.position.y - math.cosf(r) * (app.ship_height / f32(2.5))
	app.player.collider_r = 12
}

fn wrap_meteor(mut m Meteor) {
	w := f32(screen_width)
	h := f32(screen_height)
	r := m.radius
	if m.position.x > w + r {
		m.position.x = -r
	} else if m.position.x < -r {
		m.position.x = w + r
	}
	if m.position.y > h + r {
		m.position.y = -r
	} else if m.position.y < -r {
		m.position.y = h + r
	}
}

fn init_game(mut app &App) {
	app.paused = false
	app.frames_counter = 0

	// Isosceles triangle with ~70 degree base angles computed from tan(20 deg).
	app.ship_height = (player_base_size / 2) / math.tanf(f32(20) * deg2rad)
	app.player.position = gg.Vec2{f32(screen_width) / 2, f32(screen_height) / 2 - app.ship_height / 2}
	app.player.speed = gg.Vec2{}
	app.player.acceleration = 0
	app.player.rotation = 0
	app.player.color = light_gray
	update_collider(mut app)

	app.medium_meteors = []Meteor{len: max_medium_meteors}
	app.small_meteors = []Meteor{len: max_small_meteors}
	for i in 0 .. max_medium_meteors {
		app.medium_meteors[i].position = rand_meteor_pos()
		app.medium_meteors[i].speed = rand_meteor_speed()
		app.medium_meteors[i].radius = 20
		app.medium_meteors[i].active = true
		app.medium_meteors[i].color = green
	}
	for i in 0 .. max_small_meteors {
		app.small_meteors[i].position = rand_meteor_pos()
		app.small_meteors[i].speed = rand_meteor_speed()
		app.small_meteors[i].radius = 10
		app.small_meteors[i].active = true
		app.small_meteors[i].color = yellow
	}
}

fn update(mut app &App) {
	if app.game_over || app.paused {
		return
	}
	app.frames_counter++
	ctx := app.ctx

	if ctx.pressed_keys[gg.KeyCode.left] {
		app.player.rotation -= 5
	}
	if ctx.pressed_keys[gg.KeyCode.right] {
		app.player.rotation += 5
	}

	r := app.player.rotation * deg2rad
	app.player.speed.x = math.sinf(r) * player_speed
	app.player.speed.y = math.cosf(r) * player_speed

	if ctx.pressed_keys[gg.KeyCode.up] {
		if app.player.acceleration < 1 {
			app.player.acceleration += f32(0.04)
		}
	} else {
		if app.player.acceleration > 0 {
			app.player.acceleration -= f32(0.02)
		} else if app.player.acceleration < 0 {
			app.player.acceleration = 0
		}
	}
	if ctx.pressed_keys[gg.KeyCode.down] {
		if app.player.acceleration > 0 {
			app.player.acceleration -= f32(0.04)
		} else if app.player.acceleration < 0 {
			app.player.acceleration = 0
		}
	}

	app.player.position.x += app.player.speed.x * app.player.acceleration
	app.player.position.y -= app.player.speed.y * app.player.acceleration

	sh := app.ship_height
	if app.player.position.x > f32(screen_width) + sh {
		app.player.position.x = -sh
	} else if app.player.position.x < -sh {
		app.player.position.x = f32(screen_width) + sh
	}
	if app.player.position.y > f32(screen_height) + sh {
		app.player.position.y = -sh
	} else if app.player.position.y < -sh {
		app.player.position.y = f32(screen_height) + sh
	}

	update_collider(mut app)
	ship_center := gg.Vec2{app.player.collider_x, app.player.collider_y}
	for i in 0 .. max_medium_meteors {
		m := app.medium_meteors[i]
		if m.active && circles_collide(ship_center, app.player.collider_r, m.position, m.radius) {
			app.game_over = true
		}
	}
	for i in 0 .. max_small_meteors {
		m := app.small_meteors[i]
		if m.active && circles_collide(ship_center, app.player.collider_r, m.position, m.radius) {
			app.game_over = true
		}
	}

	for i in 0 .. max_medium_meteors {
		if !app.medium_meteors[i].active {
			continue
		}
		mut m := app.medium_meteors[i]
		m.position.x += m.speed.x
		m.position.y += m.speed.y
		wrap_meteor(mut m)
		app.medium_meteors[i] = m
	}
	for i in 0 .. max_small_meteors {
		if !app.small_meteors[i].active {
			continue
		}
		mut m := app.small_meteors[i]
		m.position.x += m.speed.x
		m.position.y += m.speed.y
		wrap_meteor(mut m)
		app.small_meteors[i] = m
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	update(mut app)
}

fn on_frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	if !app.game_over {
		r := app.player.rotation * deg2rad
		v1x := app.player.position.x + math.sinf(r) * app.ship_height
		v1y := app.player.position.y - math.cosf(r) * app.ship_height
		v2x := app.player.position.x - math.cosf(r) * (player_base_size / 2)
		v2y := app.player.position.y - math.sinf(r) * (player_base_size / 2)
		v3x := app.player.position.x + math.cosf(r) * (player_base_size / 2)
		v3y := app.player.position.y + math.sinf(r) * (player_base_size / 2)
		ctx.draw_triangle_filled(v1x, v1y, v2x, v2y, v3x, v3y, maroon)

		for i in 0 .. max_medium_meteors {
			m := app.medium_meteors[i]
			c := if m.active { gray } else { faded_light_gray }
			ctx.draw_circle_filled(m.position.x, m.position.y, m.radius, c)
		}
		for i in 0 .. max_small_meteors {
			m := app.small_meteors[i]
			c := if m.active { dark_gray } else { faded_light_gray }
			ctx.draw_circle_filled(m.position.x, m.position.y, m.radius, c)
		}

		t := f32(app.frames_counter) / 60
		ctx.draw_text(10, 10, 'TIME: ${t:.2f}', size: 20, color: black)
		if app.paused {
			msg := 'GAME PAUSED'
			w := ctx.text_width_cfg(msg, size: 40)
			ctx.draw_text(screen_width / 2 - w / 2, screen_height / 2 - 40, msg,
				size:  40
				color: gray
			)
		}
	} else {
		msg := 'PRESS [ENTER] TO PLAY AGAIN'
		w := ctx.text_width_cfg(msg, size: 20)
		ctx.draw_text(screen_width / 2 - w / 2, screen_height / 2 - 50, msg, size: 20, color: gray)
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
					app.game_over = false
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
		window_title: 'raylib [games] classic - asteroids survival'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
