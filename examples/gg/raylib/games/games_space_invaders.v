module main

// raylib [games] classic - space invaders, ported to gg.
// Move the paddle with the arrow keys and hold SPACE to shoot the invaders
// drifting in from the right. Clear a wave to advance; colliding with an
// invader ends the run. P pauses, ENTER restarts.

import gg
import rand

const screen_width = 800
const screen_height = 450

const num_shoots = 50
const num_max_enemies = 50
const first_wave = 10
const second_wave = 20
const third_wave = 50

const wave_first = 0
const wave_second = 1
const wave_third = 2

const alpha_step = f32(0.02)

const ray_white = gg.Color{245, 245, 245, 255}
const black = gg.Color{0, 0, 0, 255}
const gray = gg.Color{130, 130, 130, 255}
const maroon = gg.Color{127, 36, 84, 255}

struct Player {
mut:
	x      f32
	y      f32
	width  f32
	height f32
	speed  gg.Vec2
}

struct Enemy {
mut:
	x      f32
	y      f32
	width  f32
	height f32
	speed  gg.Vec2
	active bool
}

struct Shoot {
mut:
	x      f32
	y      f32
	width  f32
	height f32
	speed  gg.Vec2
	active bool
}

struct App {
mut:
	ctx            &gg.Context = unsafe { nil }
	player         Player
	enemies        []Enemy
	shoots         []Shoot
	game_over      bool
	paused         bool
	victory        bool
	score          int
	wave           int
	shoot_rate     int
	alpha          f32
	active_enemies int
	enemies_kill   int
	smooth         bool
}

fn rand_range(lo int, hi int) int {
	return lo + (rand.intn(hi - lo + 1) or { 0 })
}

fn init_game(mut app &App) {
	app.shoot_rate = 0
	app.paused = false
	app.game_over = false
	app.victory = false
	app.smooth = false
	app.wave = wave_first
	app.active_enemies = first_wave
	app.enemies_kill = 0
	app.score = 0
	app.alpha = 0

	app.player.x = 20
	app.player.y = 50
	app.player.width = 20
	app.player.height = 20
	app.player.speed = gg.Vec2{5, 5}

	app.enemies = []Enemy{len: num_max_enemies}
	for i in 0 .. num_max_enemies {
		app.enemies[i].width = 10
		app.enemies[i].height = 10
		app.enemies[i].x = f32(rand_range(screen_width, screen_width + 1000))
		app.enemies[i].y = f32(rand_range(0, screen_height - 10))
		app.enemies[i].speed = gg.Vec2{5, 5}
		app.enemies[i].active = true
	}

	app.shoots = []Shoot{len: num_shoots}
	for i in 0 .. num_shoots {
		app.shoots[i].x = app.player.x
		app.shoots[i].y = app.player.y + app.player.height / 4
		app.shoots[i].width = 10
		app.shoots[i].height = 5
		app.shoots[i].speed = gg.Vec2{7, 0}
		app.shoots[i].active = false
	}
}

fn rects_overlap(ax f32, ay f32, aw f32, ah f32, bx f32, by f32, bw f32, bh f32) bool {
	return ax < bx + bw && ax + aw > bx && ay < by + bh && ay + ah > by
}

fn respawn_enemy(mut app &App, i int) {
	app.enemies[i].x = f32(rand_range(screen_width, screen_width + 1000))
	app.enemies[i].y = f32(rand_range(0, screen_height - int(app.enemies[i].height)))
}

fn update_wave(mut app &App) {
	if !app.smooth {
		app.alpha += alpha_step
		if app.alpha >= 1 {
			app.smooth = true
		}
	} else {
		app.alpha -= alpha_step
	}

	if app.enemies_kill != app.active_enemies {
		return
	}

	if app.wave == wave_third {
		app.victory = true
		return
	}

	app.enemies_kill = 0
	for i in 0 .. app.active_enemies {
		if !app.enemies[i].active {
			app.enemies[i].active = true
		}
	}
	if app.wave == wave_first {
		app.active_enemies = second_wave
		app.wave = wave_second
	} else {
		app.active_enemies = third_wave
		app.wave = wave_third
	}
	app.smooth = false
	app.alpha = 0
}

fn update(mut app &App) {
	if app.game_over {
		return
	}
	if app.paused {
		return
	}

	update_wave(mut app)

	ctx := app.ctx
	if ctx.pressed_keys[gg.KeyCode.right] {
		app.player.x += app.player.speed.x
	}
	if ctx.pressed_keys[gg.KeyCode.left] {
		app.player.x -= app.player.speed.x
	}
	if ctx.pressed_keys[gg.KeyCode.up] {
		app.player.y -= app.player.speed.y
	}
	if ctx.pressed_keys[gg.KeyCode.down] {
		app.player.y += app.player.speed.y
	}

	for i in 0 .. app.active_enemies {
		e := app.enemies[i]
		if rects_overlap(app.player.x, app.player.y, app.player.width, app.player.height,
			e.x, e.y, e.width, e.height) {
			app.game_over = true
		}
	}

	for i in 0 .. app.active_enemies {
		if app.enemies[i].active {
			app.enemies[i].x -= app.enemies[i].speed.x
			if app.enemies[i].x < 0 {
				respawn_enemy(mut app, i)
			}
		}
	}

	if app.player.x <= 0 {
		app.player.x = 0
	}
	if app.player.x + app.player.width >= f32(screen_width) {
		app.player.x = f32(screen_width) - app.player.width
	}
	if app.player.y <= 0 {
		app.player.y = 0
	}
	if app.player.y + app.player.height >= f32(screen_height) {
		app.player.y = f32(screen_height) - app.player.height
	}

	if ctx.pressed_keys[gg.KeyCode.space] {
		app.shoot_rate += 5
		for i in 0 .. num_shoots {
			if !app.shoots[i].active && app.shoot_rate % 20 == 0 {
				app.shoots[i].x = app.player.x
				app.shoots[i].y = app.player.y + app.player.height / 4
				app.shoots[i].active = true
				break
			}
		}
	}

	for i in 0 .. num_shoots {
		if !app.shoots[i].active {
			continue
		}
		app.shoots[i].x += app.shoots[i].speed.x

		for j in 0 .. app.active_enemies {
			if !app.enemies[j].active {
				continue
			}
			sh := app.shoots[i]
			en := app.enemies[j]
			if rects_overlap(sh.x, sh.y, sh.width, sh.height, en.x, en.y, en.width, en.height) {
				app.shoots[i].active = false
				respawn_enemy(mut app, j)
				app.shoot_rate = 0
				app.enemies_kill++
				app.score += 100
			}
			if sh.x + sh.width >= f32(screen_width) {
				app.shoots[i].active = false
				app.shoot_rate = 0
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
		ctx.draw_rect_filled(app.player.x, app.player.y, app.player.width, app.player.height, black)

		msg := if app.wave == wave_first {
			'FIRST WAVE'
		} else if app.wave == wave_second {
			'SECOND WAVE'
		} else {
			'THIRD WAVE'
		}
		faded := gg.Color{0, 0, 0, int(app.alpha * 255)}
		centered_text(ctx, msg, screen_height / 2 - 40, 40, faded)

		for i in 0 .. app.active_enemies {
			e := app.enemies[i]
			if e.active {
				// PORT NOTE: raylib draws each invader as a 10x10 rectangle.
				// gg has no sprite assets here, so the invader is approximated
				// with a filled rectangle plus a pair of small "eye" cuts.
				ctx.draw_rect_filled(e.x, e.y, e.width, e.height, gray)
				ctx.draw_rect_filled(e.x + 2, e.y + 3, 2, 2, ray_white)
				ctx.draw_rect_filled(e.x + 6, e.y + 3, 2, 2, ray_white)
			}
		}

		for i in 0 .. num_shoots {
			sh := app.shoots[i]
			if sh.active {
				ctx.draw_rect_filled(sh.x, sh.y, sh.width, sh.height, maroon)
			}
		}

		ctx.draw_text(20, 20, '${app.score:04}', size: 40, color: gray)

		if app.victory {
			centered_text(ctx, 'YOU WIN', screen_height / 2 - 40, 40, black)
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
		window_title: 'raylib [games] classic - space invaders'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
