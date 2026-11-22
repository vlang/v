module main

// raylib [games] classic - missile commander, ported to gg.
// Incoming missiles fall from the top of the screen. Fire an interceptor from
// the matching launcher with the left, middle or right mouse button; it flies
// toward the cursor and destroys a missile if the two paths cross.
// Every launcher or building that is hit is destroyed; lose all of either and
// the game is over. P pauses, ENTER restarts.

import gg
import math
import rand

const screen_width = 800
const screen_height = 450

const max_missiles = 100
const max_interceptors = 30
const max_explosions = 100
const launchers_amount = 3
const buildings_amount = 6

const launcher_size = f32(80)
const building_size = f32(60)
const explosion_radius = f32(40)

const missile_speed = f32(1)
const interceptor_speed = f32(10)
const missile_launch_frames = 80
const explosion_increase_time = f32(90)
const explosion_total_time = f32(210)

const ray_white = gg.Color{245, 245, 245, 255}
const red = gg.Color{255, 0, 0, 255}
const yellow = gg.Color{255, 255, 0, 255}
const green = gg.Color{0, 255, 0, 255}
const blue = gg.Color{0, 0, 255, 255}
const gray = gg.Color{130, 130, 130, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const explosion_color = gg.Color{125, 125, 125, 125}

struct Missile {
mut:
	origin    gg.Vec2
	position  gg.Vec2
	objective gg.Vec2
	speed     gg.Vec2
	active    bool
}

struct Interceptor {
mut:
	origin    gg.Vec2
	position  gg.Vec2
	objective gg.Vec2
	speed     gg.Vec2
	active    bool
}

struct Explosion {
mut:
	position          gg.Vec2
	radius_multiplier f32
	frame             int
	active            bool
}

struct Launcher {
mut:
	position gg.Vec2
	active   bool
}

struct Building {
mut:
	position gg.Vec2
	active   bool
}

struct App {
mut:
	ctx               &gg.Context = unsafe { nil }
	missiles          []Missile
	interceptors      []Interceptor
	explosions        []Explosion
	launchers         []Launcher
	buildings         []Building
	frames_counter    int
	explosion_index   int
	interceptor_index int
	missile_index     int
	launcher_shooting int
	score             int
	mouse_x           f32
	mouse_y           f32
	game_over         bool
	paused            bool
}

fn rand_range(lo int, hi int) int {
	return lo + (rand.intn(hi - lo + 1) or { 0 })
}

fn init_game(mut app &App) {
	app.missiles = []Missile{len: max_missiles}
	app.interceptors = []Interceptor{len: max_interceptors}
	app.explosions = []Explosion{len: max_explosions}
	app.launchers = []Launcher{len: launchers_amount}
	app.buildings = []Building{len: buildings_amount}

	spacing := f32(screen_width) / f32(launchers_amount + buildings_amount + 1)
	base_y := f32(screen_height)
	app.launchers[0].position = gg.Vec2{1 * spacing, base_y - launcher_size / 2}
	app.buildings[0].position = gg.Vec2{2 * spacing, base_y - building_size / 2}
	app.buildings[1].position = gg.Vec2{3 * spacing, base_y - building_size / 2}
	app.buildings[2].position = gg.Vec2{4 * spacing, base_y - building_size / 2}
	app.launchers[1].position = gg.Vec2{5 * spacing, base_y - launcher_size / 2}
	app.buildings[3].position = gg.Vec2{6 * spacing, base_y - building_size / 2}
	app.buildings[4].position = gg.Vec2{7 * spacing, base_y - building_size / 2}
	app.buildings[5].position = gg.Vec2{8 * spacing, base_y - building_size / 2}
	app.launchers[2].position = gg.Vec2{9 * spacing, base_y - launcher_size / 2}

	for i in 0 .. launchers_amount {
		app.launchers[i].active = true
	}
	for i in 0 .. buildings_amount {
		app.buildings[i].active = true
	}

	app.frames_counter = 0
	app.explosion_index = 0
	app.interceptor_index = 0
	app.missile_index = 0
	app.launcher_shooting = 0
	app.score = 0
	app.paused = false
}

fn add_explosion(mut app &App, at gg.Vec2) {
	i := app.explosion_index
	app.explosions[i].position = at
	app.explosions[i].radius_multiplier = 0
	app.explosions[i].frame = 0
	app.explosions[i].active = true
	app.explosion_index++
	if app.explosion_index == max_explosions {
		app.explosion_index = 0
	}
}

// direction_speed mirrors the raylib code's objective - origin normalisation.
// A zero-length objective is treated as no movement instead of producing NaN.
fn direction_speed(from gg.Vec2, to gg.Vec2, speed f32) gg.Vec2 {
	dx := to.x - from.x
	dy := to.y - from.y
	length := math.sqrtf(dx * dx + dy * dy)
	if length <= 0 {
		return gg.Vec2{}
	}
	return gg.Vec2{dx * speed / length, dy * speed / length}
}

fn point_in_square(p gg.Vec2, center_x f32, center_y f32, size f32) bool {
	half := size / 2
	return p.x >= center_x - half && p.x <= center_x + half
		&& p.y >= center_y - half && p.y <= center_y + half
}

fn update_outgoing_fire(mut app &App) {
	shooter := app.launcher_shooting
	app.launcher_shooting = 0
	if shooter <= 0 || shooter > launchers_amount {
		return
	}
	launcher := app.launchers[shooter - 1]
	if !launcher.active {
		return
	}
	i := app.interceptor_index
	mut it := app.interceptors[i]
	it.active = true
	it.origin = launcher.position
	it.position = it.origin
	it.objective = gg.Vec2{app.mouse_x, app.mouse_y}
	it.speed = direction_speed(it.origin, it.objective, interceptor_speed)
	app.interceptors[i] = it
	app.interceptor_index++
	if app.interceptor_index == max_interceptors {
		app.interceptor_index = 0
	}
}

fn update_incoming_fire(mut app &App) {
	if app.frames_counter % missile_launch_frames != 0 {
		return
	}
	i := app.missile_index
	mut m := app.missiles[i]
	m.active = true
	m.origin = gg.Vec2{f32(rand_range(20, screen_width - 20)), -10}
	m.position = m.origin
	m.objective = gg.Vec2{f32(rand_range(20, screen_width - 20)), f32(screen_height + 10)}
	m.speed = direction_speed(m.origin, m.objective, missile_speed)
	app.missiles[i] = m
	app.missile_index++
	if app.missile_index == max_missiles {
		app.missile_index = 0
	}
}

fn update(mut app &App) {
	app.frames_counter++

	for i in 0 .. max_interceptors {
		if !app.interceptors[i].active {
			continue
		}
		mut it := app.interceptors[i]
		it.position = it.position + it.speed
		dx := it.position.x - it.objective.x
		dy := it.position.y - it.objective.y
		if math.sqrtf(dx * dx + dy * dy) < interceptor_speed {
			it.active = false
			app.interceptors[i] = it
			add_explosion(mut app, it.position)
			break
		}
		app.interceptors[i] = it
	}

	for i in 0 .. max_missiles {
		if !app.missiles[i].active {
			continue
		}
		mut m := app.missiles[i]
		m.position = m.position + m.speed
		if m.position.y > f32(screen_height) {
			m.active = false
			app.missiles[i] = m
			continue
		}
		mut hit := false
		for j in 0 .. launchers_amount {
			l := app.launchers[j]
			if l.active && point_in_square(m.position, l.position.x, l.position.y, launcher_size) {
				app.launchers[j].active = false
				hit = true
				break
			}
		}
		if !hit {
			for j in 0 .. buildings_amount {
				b := app.buildings[j]
				if b.active && point_in_square(m.position, b.position.x, b.position.y, building_size) {
					app.buildings[j].active = false
					hit = true
					break
				}
			}
		}
		if !hit {
			for j in 0 .. max_explosions {
				ex := app.explosions[j]
				if !ex.active {
					continue
				}
				radius := explosion_radius * ex.radius_multiplier
				dx := m.position.x - ex.position.x
				dy := m.position.y - ex.position.y
				if math.sqrtf(dx * dx + dy * dy) <= radius {
					app.score += 100
					hit = true
					break
				}
			}
		}
		if hit {
			m.active = false
			add_explosion(mut app, m.position)
		}
		app.missiles[i] = m
	}

	for i in 0 .. max_explosions {
		if !app.explosions[i].active {
			continue
		}
		mut ex := app.explosions[i]
		ex.frame++
		f := f32(ex.frame)
		if f <= explosion_increase_time {
			ex.radius_multiplier = f / explosion_increase_time
		} else if f <= explosion_total_time {
			ex.radius_multiplier = 1 - (f - explosion_increase_time) / explosion_total_time
		} else {
			ex.frame = 0
			ex.active = false
		}
		app.explosions[i] = ex
	}

	update_outgoing_fire(mut app)
	update_incoming_fire(mut app)

	mut destroyed := 0
	for i in 0 .. launchers_amount {
		if !app.launchers[i].active {
			destroyed++
		}
	}
	if destroyed == launchers_amount {
		app.game_over = true
	}
	destroyed = 0
	for i in 0 .. buildings_amount {
		if !app.buildings[i].active {
			destroyed++
		}
	}
	if destroyed == buildings_amount {
		app.game_over = true
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.game_over || app.paused {
		return
	}
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
		for i in 0 .. max_missiles {
			m := app.missiles[i]
			if !m.active {
				continue
			}
			ctx.draw_line(m.origin.x, m.origin.y, m.position.x, m.position.y, red)
			if app.frames_counter % 16 < 8 {
				ctx.draw_circle_filled(m.position.x, m.position.y, 3, yellow)
			}
		}
		for i in 0 .. max_interceptors {
			it := app.interceptors[i]
			if !it.active {
				continue
			}
			ctx.draw_line(it.origin.x, it.origin.y, it.position.x, it.position.y, green)
			if app.frames_counter % 16 < 8 {
				ctx.draw_circle_filled(it.position.x, it.position.y, 3, blue)
			}
		}
		for i in 0 .. max_explosions {
			ex := app.explosions[i]
			if !ex.active {
				continue
			}
			ctx.draw_circle_filled(ex.position.x, ex.position.y,
				explosion_radius * ex.radius_multiplier, explosion_color)
		}
		for i in 0 .. launchers_amount {
			l := app.launchers[i]
			if l.active {
				ctx.draw_rect_filled(l.position.x - launcher_size / 2,
					l.position.y - launcher_size / 2, launcher_size, launcher_size, gray)
			}
		}
		for i in 0 .. buildings_amount {
			b := app.buildings[i]
			if b.active {
				ctx.draw_rect_filled(b.position.x - building_size / 2,
					b.position.y - building_size / 2, building_size, building_size, light_gray)
			}
		}
		ctx.draw_text(20, 20, 'SCORE ${app.score}', size: 40, color: light_gray)
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
	app.mouse_x = f32(e.mouse_x)
	app.mouse_y = f32(e.mouse_y)
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
			else {}
		}
		return
	}
	if e.typ == .mouse_down {
		match e.mouse_button {
			.left { app.launcher_shooting = 1 }
			.middle { app.launcher_shooting = 2 }
			.right { app.launcher_shooting = 3 }
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
		window_title: 'raylib [games] classic - missile commander'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
