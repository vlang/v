module main

// raylib [games] classic - gorilas, ported to gg.
// Two teams of gorillas take turns lobbing explosive bananas from rooftop to
// rooftop. Aim with the mouse inside your firing quadrant and click to shoot;
// gravity curves the banana. A hit destroys a gorilla or carves a crater into a
// building. When one team runs out of gorillas the game is over.
// P pauses, ENTER restarts. Sprites are approximated with gg primitives.

import gg
import math
import rand

const screen_width = 800
const screen_height = 450

const max_buildings = 15
const max_explosions = 200
const max_players = 2

const building_relative_error = 30
const building_min_relative_height = 20
const building_max_relative_height = 60
const building_min_grayscale_color = 120
const building_max_grayscale_color = 200
const min_player_position = 5
const max_player_position = 20

const gravity = f32(9.81)
const delta_fps = f32(60)
const ball_radius = 10
const explosion_default_radius = 30

const deg2rad = f32(0.017453292519943295)
const rad2deg = f64(57.29577951308232)

const ray_white = gg.Color{245, 245, 245, 255}
const blue = gg.Color{0, 0, 255, 255}
const red = gg.Color{255, 0, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}
const gray = gg.Color{130, 130, 130, 255}
const dark_blue = gg.Color{0, 0, 139, 255}

struct Player {
mut:
	position       gg.Vec2
	size           gg.Vec2
	aiming_point   gg.Vec2
	aiming_angle   int
	aiming_power   int
	previous_point gg.Vec2
	previous_angle int
	previous_power int
	impact_point   gg.Vec2
	is_left_team   bool
	is_player      bool
	is_alive       bool
}

struct Building {
mut:
	x     f32
	y     f32
	w     f32
	h     f32
	color gg.Color
}

struct Explosion {
mut:
	position gg.Vec2
	radius   int
	active   bool
}

struct Ball {
mut:
	position gg.Vec2
	speed    gg.Vec2
	radius   int
	active   bool
}

struct App {
mut:
	ctx              &gg.Context = unsafe { nil }
	players          []Player
	buildings        []Building
	explosions       []Explosion
	ball             Ball
	player_turn      int
	ball_on_air      bool
	game_over        bool
	paused           bool
	mouse_x          f32
	mouse_y          f32
	fire_pressed     bool
	explosion_number int
}

fn rand_range(lo int, hi int) int {
	return lo + (rand.intn(hi - lo + 1) or { 0 })
}

fn init_buildings(mut app &App) {
	relative_width := f32(100) / f32(100 - building_relative_error)
	building_width_mean := int(f32(screen_width) * relative_width / f32(max_buildings)) + 1
	mut current_width := 0
	for i in 0 .. max_buildings {
		lo := int(f32(building_width_mean) * f32(100 - building_relative_error / 2) / 100) + 1
		hi := int(f32(building_width_mean) * f32(100 + building_relative_error) / 100)
		w := rand_range(lo, hi)
		app.buildings[i].x = f32(current_width)
		app.buildings[i].w = f32(w)
		current_width += w
		rh := rand_range(building_min_relative_height, building_max_relative_height)
		hpx := screen_height * rh / 100
		app.buildings[i].y = f32(screen_height - hpx)
		app.buildings[i].h = f32(hpx + 1)
		level := rand_range(building_min_grayscale_color, building_max_grayscale_color)
		app.buildings[i].color = gg.Color{level, level, level, 255}
	}
}

fn init_players(mut app &App) {
	for i in 0 .. max_players {
		mut p := app.players[i]
		p.is_alive = true
		p.is_left_team = (i % 2) == 0
		p.is_player = true
		p.size = gg.Vec2{40, 40}
		if p.is_left_team {
			p.position.x = f32(rand_range(screen_width * min_player_position / 100,
				screen_width * max_player_position / 100))
		} else {
			p.position.x = f32(screen_width - rand_range(screen_width * min_player_position / 100,
				screen_width * max_player_position / 100))
		}
		// Start at j = 1 because building[0] always starts at x = 0, so the
		// C original's j - 1 access can only ever refer to a valid building.
		for j in 1 .. max_buildings {
			if app.buildings[j].x > p.position.x {
				p.position.x = app.buildings[j - 1].x + app.buildings[j - 1].w / 2
				p.position.y = app.buildings[j - 1].y - p.size.y / 2
				break
			}
		}
		p.aiming_point = p.position
		p.previous_point = p.position
		p.previous_angle = 0
		p.previous_power = 0
		p.aiming_angle = 0
		p.aiming_power = 0
		p.impact_point = gg.Vec2{-100, -100}
		app.players[i] = p
	}
}

fn init_game(mut app &App) {
	app.players = []Player{len: max_players}
	app.buildings = []Building{len: max_buildings}
	app.explosions = []Explosion{len: max_explosions}

	app.ball.radius = ball_radius
	app.ball_on_air = false
	app.ball.active = false

	init_buildings(mut app)
	init_players(mut app)

	for i in 0 .. max_explosions {
		app.explosions[i].position = gg.Vec2{}
		app.explosions[i].radius = explosion_default_radius
		app.explosions[i].active = false
	}
	app.paused = false
	// PORT NOTE: explosion_number is a static counter in the C version and is
	// deliberately not reset here; it is wrapped modulo max_explosions on use.
}

fn circles_collide(c1 gg.Vec2, r1 f32, c2 gg.Vec2, r2 f32) bool {
	dx := c1.x - c2.x
	dy := c1.y - c2.y
	rr := r1 + r2
	return dx * dx + dy * dy <= rr * rr
}

fn circle_rec_collide(c gg.Vec2, r f32, x f32, y f32, w f32, h f32) bool {
	nx := if c.x < x {
		x
	} else if c.x > x + w {
		x + w
	} else {
		c.x
	}
	ny := if c.y < y {
		y
	} else if c.y > y + h {
		y + h
	} else {
		c.y
	}
	dx := c.x - nx
	dy := c.y - ny
	return dx * dx + dy * dy <= r * r
}

// angle_from_dy mirrors the C code's asin((player.y - mouse.y) / power) * RAD2DEG.
fn angle_from_dy(dy f32, power int) int {
	if power <= 0 {
		return 0
	}
	mut v := f64(dy) / f64(power)
	// PORT NOTE: clamp to asin's domain; the C version left NaN undefined when
	// the int-truncated power ended up slightly smaller than |dy|.
	if v > 1.0 {
		v = 1.0
	}
	if v < -1.0 {
		v = -1.0
	}
	return int(math.asin(v) * rad2deg)
}

fn update_player(mut app &App, turn int, fired bool) bool {
	mut p := app.players[turn]
	if app.mouse_y <= p.position.y {
		if p.is_left_team && app.mouse_x >= p.position.x {
			dx := p.position.x - app.mouse_x
			dy := p.position.y - app.mouse_y
			p.aiming_power = int(math.sqrtf(dx * dx + dy * dy))
			p.aiming_angle = angle_from_dy(dy, p.aiming_power)
			p.aiming_point = gg.Vec2{app.mouse_x, app.mouse_y}
			if fired {
				p.previous_point = p.aiming_point
				p.previous_power = p.aiming_power
				p.previous_angle = p.aiming_angle
				app.ball.position = p.position
				app.players[turn] = p
				return true
			}
		} else if !p.is_left_team && app.mouse_x <= p.position.x {
			dx := p.position.x - app.mouse_x
			dy := p.position.y - app.mouse_y
			p.aiming_power = int(math.sqrtf(dx * dx + dy * dy))
			p.aiming_angle = angle_from_dy(dy, p.aiming_power)
			p.aiming_point = gg.Vec2{app.mouse_x, app.mouse_y}
			if fired {
				p.previous_point = p.aiming_point
				p.previous_power = p.aiming_power
				p.previous_angle = p.aiming_angle
				app.ball.position = p.position
				app.players[turn] = p
				return true
			}
		} else {
			p.aiming_point = p.position
			p.aiming_power = 0
			p.aiming_angle = 0
		}
	} else {
		p.aiming_point = p.position
		p.aiming_power = 0
		p.aiming_angle = 0
	}
	app.players[turn] = p
	return false
}

fn update_ball(mut app &App, turn int) bool {
	if !app.ball.active {
		angle := f32(app.players[turn].previous_angle)
		power := f32(app.players[turn].previous_power)
		ca := math.cosf(angle * deg2rad)
		sa := math.sinf(angle * deg2rad)
		if app.players[turn].is_left_team {
			app.ball.speed.x = ca * power * 3 / delta_fps
			app.ball.speed.y = -sa * power * 3 / delta_fps
		} else {
			app.ball.speed.x = -ca * power * 3 / delta_fps
			app.ball.speed.y = -sa * power * 3 / delta_fps
		}
		app.ball.active = true
	}

	app.ball.position.x += app.ball.speed.x
	app.ball.position.y += app.ball.speed.y
	app.ball.speed.y += gravity / delta_fps

	r := f32(app.ball.radius)
	if app.ball.position.x + r < 0 {
		return true
	} else if app.ball.position.x - r > f32(screen_width) {
		return true
	}

	for i in 0 .. max_players {
		p := app.players[i]
		if circle_rec_collide(app.ball.position, r, p.position.x - p.size.x / 2,
			p.position.y - p.size.y / 2, p.size.x, p.size.y) {
			// A gorilla cannot hit itself.
			if i == turn {
				return false
			}
			app.players[turn].impact_point = gg.Vec2{app.ball.position.x, app.ball.position.y + r}
			app.players[i].is_alive = false
			return true
		}
	}

	// PORT NOTE: an explosion is an active white circle, not a damage radius;
	// passing through one is how craters are carved out of buildings.
	for i in 0 .. max_explosions {
		ex := app.explosions[i]
		if circles_collide(app.ball.position, r, ex.position, f32(ex.radius) - r) {
			return false
		}
	}

	for i in 0 .. max_buildings {
		b := app.buildings[i]
		if circle_rec_collide(app.ball.position, r, b.x, b.y, b.w, b.h) {
			app.players[turn].impact_point = gg.Vec2{app.ball.position.x, app.ball.position.y + r}
			idx := app.explosion_number % max_explosions
			app.explosions[idx].position = app.players[turn].impact_point
			app.explosions[idx].active = true
			app.explosion_number++
			return true
		}
	}
	return false
}

fn update(mut app &App) {
	if app.game_over || app.paused {
		return
	}
	fired := app.fire_pressed
	app.fire_pressed = false
	if !app.ball_on_air {
		app.ball_on_air = update_player(mut app, app.player_turn, fired)
	} else if update_ball(mut app, app.player_turn) {
		mut left_alive := false
		mut right_alive := false
		for i in 0 .. max_players {
			p := app.players[i]
			if p.is_alive {
				if p.is_left_team {
					left_alive = true
				} else {
					right_alive = true
				}
			}
		}
		if left_alive && right_alive {
			app.ball_on_air = false
			app.ball.active = false
			app.player_turn++
			if app.player_turn == max_players {
				app.player_turn = 0
			}
		} else {
			app.game_over = true
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
		for i in 0 .. max_buildings {
			b := app.buildings[i]
			ctx.draw_rect_filled(b.x, b.y, b.w, b.h, b.color)
		}
		for i in 0 .. max_explosions {
			ex := app.explosions[i]
			if ex.active {
				ctx.draw_circle_filled(ex.position.x, ex.position.y, f32(ex.radius), ray_white)
			}
		}
		for i in 0 .. max_players {
			p := app.players[i]
			if p.is_alive {
				c := if p.is_left_team { blue } else { red }
				ctx.draw_rect_filled(p.position.x - p.size.x / 2, p.position.y - p.size.y / 2,
					p.size.x, p.size.y, c)
			}
		}
		if app.ball.active {
			ctx.draw_circle_filled(app.ball.position.x, app.ball.position.y, f32(app.ball.radius), maroon)
		}
		if !app.ball_on_air {
			p := app.players[app.player_turn]
			if p.is_left_team {
				ctx.draw_triangle_filled(p.position.x - p.size.x / 4, p.position.y - p.size.y / 4,
					p.position.x + p.size.x / 4, p.position.y + p.size.y / 4,
					p.previous_point.x, p.previous_point.y, gray)
				ctx.draw_triangle_filled(p.position.x - p.size.x / 4, p.position.y - p.size.y / 4,
					p.position.x + p.size.x / 4, p.position.y + p.size.y / 4,
					p.aiming_point.x, p.aiming_point.y, dark_blue)
			} else {
				ctx.draw_triangle_filled(p.position.x - p.size.x / 4, p.position.y + p.size.y / 4,
					p.position.x + p.size.x / 4, p.position.y - p.size.y / 4,
					p.previous_point.x, p.previous_point.y, gray)
				ctx.draw_triangle_filled(p.position.x - p.size.x / 4, p.position.y + p.size.y / 4,
					p.position.x + p.size.x / 4, p.position.y - p.size.y / 4,
					p.aiming_point.x, p.aiming_point.y, maroon)
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
	app.mouse_x = e.mouse_x
	app.mouse_y = e.mouse_y
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
		return
	}
	if e.typ == .mouse_down && e.mouse_button == .left {
		app.fire_pressed = true
	}
}

fn main() {
	mut app := &App{}
	init_game(mut app)
	app.ctx = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [games] classic - gorilas'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
