module main

// raylib [games] classic - asteroids, ported to gg.
// Steer a small ship with LEFT/RIGHT, thrust with UP, brake with DOWN and fire
// with SPACE. Big rocks split into medium ones, medium into small ones. Clear
// every rock to win; touching a rock ends the run. P pauses, ENTER restarts.

import gg
import math
import rand

const screen_width = 800
const screen_height = 450

const player_base_size = f32(20)
const player_speed = f32(6)
const player_max_shoots = 10
const meteors_speed = f32(2)

const max_big_meteors = 4
const max_medium_meteors = 8
const max_small_meteors = 16
const total_meteors = max_big_meteors + max_medium_meteors + max_small_meteors

const player_rotation_speed = f32(5)
const thrust_increase = f32(0.04)
const thrust_decrease = f32(0.02)
const shoot_life = 60
const collider_radius = f32(12)

// DEG2RAD, kept as f32 so rotation maths stays in single precision.
const deg2rad = f32(0.017453292519943295)

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{127, 36, 84, 255}
const faded_light_gray = gg.Color{200, 200, 200, 76}

struct Player {
mut:
	position     gg.Vec2
	speed        gg.Vec2
	acceleration f32
	rotation     f32
	collider     gg.Vec2
	collider_rad f32
}

struct Shoot {
mut:
	position   gg.Vec2
	speed      gg.Vec2
	radius     f32
	rotation   f32
	life_spawn int
	active     bool
}

struct Meteor {
mut:
	position gg.Vec2
	speed    gg.Vec2
	radius   f32
	// PORT NOTE: raylib draws rocks as plain circles. A fixed rotation is kept
	// here so each rock can be drawn as a slightly different polygon instead.
	rotation f32
	active   bool
}

struct App {
mut:
	ctx                     &gg.Context = unsafe { nil }
	player                  Player
	shoots                  []Shoot
	big_meteors             []Meteor
	medium_meteors          []Meteor
	small_meteors           []Meteor
	ship_height             f32
	mid_meteors_count       int
	small_meteors_count     int
	destroyed_meteors_count int
	game_over               bool
	paused                  bool
	victory                 bool
	shoot_requested         bool
}

fn rand_range(lo int, hi int) int {
	return lo + (rand.intn(hi - lo + 1) or { 0 })
}

// A big rock starts away from the centre of the screen, as in the raylib game.
fn big_rock_position() gg.Vec2 {
	for _ in 0 .. 100 {
		x := f32(rand_range(0, screen_width))
		if x > f32(screen_width) / 2 - 150 && x < f32(screen_width) / 2 + 150 {
			continue
		}
		y := f32(rand_range(0, screen_height))
		if y > f32(screen_height) / 2 - 150 && y < f32(screen_height) / 2 + 150 {
			continue
		}
		return gg.Vec2{x, y}
	}
	return gg.Vec2{0, 0}
}

// Rocks never start perfectly still.
fn rock_velocity() gg.Vec2 {
	for _ in 0 .. 100 {
		vx := f32(rand_range(-2, 2))
		vy := f32(rand_range(-2, 2))
		if vx != 0 || vy != 0 {
			return gg.Vec2{vx, vy}
		}
	}
	return gg.Vec2{1, 0}
}

fn init_meteor_array(count int, radius f32, active bool) []Meteor {
	mut result := []Meteor{len: count}
	for i in 0 .. count {
		result[i].position = gg.Vec2{-100, -100}
		result[i].speed = gg.Vec2{}
		result[i].radius = radius
		result[i].rotation = f32(rand_range(0, 360))
		result[i].active = active
	}
	return result
}

fn init_game(mut app &App) {
	app.ship_height = (player_base_size / 2) / math.tanf(20 * deg2rad)

	app.player.position = gg.Vec2{f32(screen_width) / 2, f32(screen_height) / 2 - app.ship_height / 2}
	app.player.speed = gg.Vec2{}
	app.player.acceleration = 0
	app.player.rotation = 0
	app.player.collider = gg.Vec2{app.player.position.x, app.player.position.y - app.ship_height / 2.5}
	app.player.collider_rad = collider_radius

	app.shoots = []Shoot{len: player_max_shoots}
	for i in 0 .. player_max_shoots {
		app.shoots[i].position = gg.Vec2{}
		app.shoots[i].speed = gg.Vec2{}
		app.shoots[i].radius = 2
		app.shoots[i].rotation = 0
		app.shoots[i].life_spawn = 0
		app.shoots[i].active = false
	}

	app.big_meteors = init_meteor_array(max_big_meteors, 40, true)
	for i in 0 .. max_big_meteors {
		app.big_meteors[i].position = big_rock_position()
		app.big_meteors[i].speed = rock_velocity()
	}
	app.medium_meteors = init_meteor_array(max_medium_meteors, 20, false)
	app.small_meteors = init_meteor_array(max_small_meteors, 10, false)

	app.mid_meteors_count = 0
	app.small_meteors_count = 0
	app.destroyed_meteors_count = 0
	app.shoot_requested = false
	app.game_over = false
	app.paused = false
	app.victory = false
}

fn circles_overlap(a gg.Vec2, ar f32, b gg.Vec2, br f32) bool {
	dx := a.x - b.x
	dy := a.y - b.y
	r := ar + br
	return dx * dx + dy * dy <= r * r
}

fn wrap_position(pos gg.Vec2, radius f32) gg.Vec2 {
	mut p := pos
	if p.x > f32(screen_width) + radius {
		p.x = -radius
	} else if p.x < -radius {
		p.x = f32(screen_width) + radius
	}
	if p.y > f32(screen_height) + radius {
		p.y = -radius
	} else if p.y < -radius {
		p.y = f32(screen_height) + radius
	}
	return p
}

fn fire_shoot(mut app &App) {
	rot := app.player.rotation * deg2rad
	for i in 0 .. player_max_shoots {
		if !app.shoots[i].active {
			app.shoots[i].position = gg.Vec2{app.player.position.x + math.sinf(rot) * app.ship_height, app.player.position.y - math.cosf(rot) * app.ship_height}
			app.shoots[i].speed = gg.Vec2{1.5 * math.sinf(rot) * player_speed, 1.5 * math.cosf(rot) * player_speed}
			app.shoots[i].rotation = app.player.rotation
			app.shoots[i].life_spawn = 0
			app.shoots[i].active = true
			break
		}
	}
}

// A destroyed rock throws off two smaller rocks travelling in opposite
// directions along the angle of the shot that split it.
fn split_rocks(mut dest []Meteor, count int, at gg.Vec2, shoot_rotation f32) int {
	mut n := count
	for _ in 0 .. 2 {
		i := n
		mut sign := f32(1)
		if n % 2 == 0 {
			sign = -1
		}
		rot := shoot_rotation * deg2rad
		dest[i].position = at
		dest[i].speed = gg.Vec2{math.cosf(rot) * meteors_speed * sign, math.sinf(rot) * meteors_speed * sign}
		dest[i].rotation = f32(rand_range(0, 360))
		dest[i].active = true
		n++
	}
	return n
}

fn update_player(mut app &App) {
	ctx := app.ctx
	if ctx.pressed_keys[gg.KeyCode.left] {
		app.player.rotation -= player_rotation_speed
	}
	if ctx.pressed_keys[gg.KeyCode.right] {
		app.player.rotation += player_rotation_speed
	}

	rot := app.player.rotation * deg2rad
	app.player.speed = gg.Vec2{math.sinf(rot) * player_speed, math.cosf(rot) * player_speed}

	if ctx.pressed_keys[gg.KeyCode.up] {
		if app.player.acceleration < 1 {
			app.player.acceleration += thrust_increase
		}
	} else if app.player.acceleration > 0 {
		app.player.acceleration -= thrust_decrease
	} else if app.player.acceleration < 0 {
		app.player.acceleration = 0
	}
	if ctx.pressed_keys[gg.KeyCode.down] {
		if app.player.acceleration > 0 {
			app.player.acceleration -= thrust_increase
		} else if app.player.acceleration < 0 {
			app.player.acceleration = 0
		}
	}

	app.player.position.x += app.player.speed.x * app.player.acceleration
	app.player.position.y -= app.player.speed.y * app.player.acceleration
	app.player.position = wrap_position(app.player.position, app.ship_height)

	app.player.collider = gg.Vec2{app.player.position.x + math.sinf(rot) * (app.ship_height / 2.5), app.player.position.y - math.cosf(rot) * (app.ship_height / 2.5)}
}

fn update_shoots(mut app &App) {
	for i in 0 .. player_max_shoots {
		if !app.shoots[i].active {
			continue
		}
		app.shoots[i].life_spawn++
		app.shoots[i].position.x += app.shoots[i].speed.x
		app.shoots[i].position.y -= app.shoots[i].speed.y

		mut s := app.shoots[i]
		if s.position.x > f32(screen_width) + s.radius || s.position.x < -s.radius
			|| s.position.y > f32(screen_height) + s.radius || s.position.y < -s.radius {
			s.active = false
			s.life_spawn = 0
		}
		if s.life_spawn >= shoot_life {
			s.position = gg.Vec2{}
			s.speed = gg.Vec2{}
			s.life_spawn = 0
			s.active = false
		}
		app.shoots[i] = s
	}
}

fn update_rocks(mut app &App) {
	for i in 0 .. max_big_meteors {
		if app.big_meteors[i].active {
			mut m := app.big_meteors[i]
			m.position.x += m.speed.x
			m.position.y += m.speed.y
			m.position = wrap_position(m.position, m.radius)
			app.big_meteors[i] = m
		}
	}
	for i in 0 .. max_medium_meteors {
		if app.medium_meteors[i].active {
			mut m := app.medium_meteors[i]
			m.position.x += m.speed.x
			m.position.y += m.speed.y
			m.position = wrap_position(m.position, m.radius)
			app.medium_meteors[i] = m
		}
	}
	for i in 0 .. max_small_meteors {
		if app.small_meteors[i].active {
			mut m := app.small_meteors[i]
			m.position.x += m.speed.x
			m.position.y += m.speed.y
			m.position = wrap_position(m.position, m.radius)
			app.small_meteors[i] = m
		}
	}
}

fn update(mut app &App) {
	if app.game_over {
		return
	}

	if app.shoot_requested {
		app.shoot_requested = false
		if !app.paused {
			fire_shoot(mut app)
		}
	}

	if !app.paused {
		update_player(mut app)
		update_shoots(mut app)

		// Player versus rocks.
		for i in 0 .. max_big_meteors {
			m := app.big_meteors[i]
			if m.active && circles_overlap(app.player.collider, app.player.collider_rad, m.position, m.radius) {
				app.game_over = true
			}
		}
		for i in 0 .. max_medium_meteors {
			m := app.medium_meteors[i]
			if m.active && circles_overlap(app.player.collider, app.player.collider_rad, m.position, m.radius) {
				app.game_over = true
			}
		}
		for i in 0 .. max_small_meteors {
			m := app.small_meteors[i]
			if m.active && circles_overlap(app.player.collider, app.player.collider_rad, m.position, m.radius) {
				app.game_over = true
			}
		}

		update_rocks(mut app)

		// Shots versus rocks. A shot that destroys a big rock may immediately
		// hit one of the freshly spawned fragments, matching the C game.
		for i in 0 .. player_max_shoots {
			if !app.shoots[i].active {
				continue
			}
			shot_pos := app.shoots[i].position
			shot_rad := app.shoots[i].radius
			shot_rot := app.shoots[i].rotation

			for a in 0 .. max_big_meteors {
				m := app.big_meteors[a]
				if m.active && circles_overlap(shot_pos, shot_rad, m.position, m.radius) {
					app.shoots[i].active = false
					app.shoots[i].life_spawn = 0
					app.big_meteors[a].active = false
					app.destroyed_meteors_count++
					app.mid_meteors_count = split_rocks(mut app.medium_meteors, app.mid_meteors_count, m.position, shot_rot)
					break
				}
			}
			for b in 0 .. max_medium_meteors {
				m := app.medium_meteors[b]
				if m.active && circles_overlap(shot_pos, shot_rad, m.position, m.radius) {
					app.shoots[i].active = false
					app.shoots[i].life_spawn = 0
					app.medium_meteors[b].active = false
					app.destroyed_meteors_count++
					app.small_meteors_count = split_rocks(mut app.small_meteors, app.small_meteors_count, m.position, shot_rot)
					break
				}
			}
			for c in 0 .. max_small_meteors {
				m := app.small_meteors[c]
				if m.active && circles_overlap(shot_pos, shot_rad, m.position, m.radius) {
					app.shoots[i].active = false
					app.shoots[i].life_spawn = 0
					app.small_meteors[c].active = false
					app.destroyed_meteors_count++
					break
				}
			}
		}
	}

	if app.destroyed_meteors_count == total_meteors {
		app.victory = true
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

fn draw_rock(ctx &gg.Context, m Meteor, active_color gg.Color) {
	c := if m.active { active_color } else { faded_light_gray }
	// PORT NOTE: raylib draws rocks as filled circles (DrawCircleV). gg has a
	// polygon primitive, so each rock is a seven-sided polygon with its own
	// rotation to suggest a jagged rock silhouette without any image assets.
	ctx.draw_polygon_filled(m.position.x, m.position.y, m.radius, 7, m.rotation, c)
}

fn on_frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()

	if !app.game_over {
		rot := app.player.rotation * deg2rad
		v1 := gg.Vec2{app.player.position.x + math.sinf(rot) * app.ship_height, app.player.position.y - math.cosf(rot) * app.ship_height}
		v2 := gg.Vec2{app.player.position.x - math.cosf(rot) * (player_base_size / 2), app.player.position.y - math.sinf(rot) * (player_base_size / 2)}
		v3 := gg.Vec2{app.player.position.x + math.cosf(rot) * (player_base_size / 2), app.player.position.y + math.sinf(rot) * (player_base_size / 2)}
		ctx.draw_triangle_filled(v1.x, v1.y, v2.x, v2.y, v3.x, v3.y, maroon)

		for i in 0 .. player_max_shoots {
			s := app.shoots[i]
			if s.active {
				ctx.draw_circle_filled(s.position.x, s.position.y, s.radius, gg.black)
			}
		}

		for i in 0 .. max_big_meteors {
			draw_rock(ctx, app.big_meteors[i], dark_gray)
		}
		for i in 0 .. max_medium_meteors {
			draw_rock(ctx, app.medium_meteors[i], gray)
		}
		for i in 0 .. max_small_meteors {
			draw_rock(ctx, app.small_meteors[i], gray)
		}

		if app.victory {
			centered_text(ctx, 'VICTORY', screen_height / 2, 20, light_gray)
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
	if e.typ != .key_down || e.key_repeat {
		return
	}
	match e.key_code {
		.p {
			if !app.game_over {
				app.paused = !app.paused
			}
		}
		.space {
			if !app.game_over && !app.paused {
				app.shoot_requested = true
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

fn main() {
	mut app := &App{}
	init_game(mut app)
	app.ctx = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [games] classic - asteroids'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
