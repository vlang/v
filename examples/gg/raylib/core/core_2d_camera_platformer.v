module main

// raylib [core] example - 2d camera platformer
// A small platformer demonstrating five different 2D camera behaviours.
// Arrows move, SPACE jumps, C cycles camera modes, R resets, wheel zooms.

import gg
import math

const screen_width = 800
const screen_height = 450
const gravity = f32(400)
const player_jump_speed = f32(350)
const player_horizontal_speed = f32(200)
const max_env_items = 5

const ray_white = gg.Color{245, 245, 245, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const gray = gg.Color{130, 130, 130, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const black = gg.Color{0, 0, 0, 255}
const gold = gg.Color{255, 203, 0, 255}

struct Player {
mut:
	position gg.Vec2 = gg.Vec2{400, 280}
	speed    f32
	can_jump bool
}

struct EnvItem {
mut:
	x        f32
	y        f32
	width    f32
	height   f32
	blocking bool
	color    gg.Color
}

struct App {
mut:
	gg              &gg.Context = unsafe { nil }
	player          Player
	env_items       [max_env_items]EnvItem
	cam             gg.Camera2D
	camera_mode     int
	evening_out     bool
	even_out_target f32
	look_timer      f32
}

fn main() {
	mut app := &App{}
	app.env_items = [
		EnvItem{0, 0, 1000, 400, false, light_gray},
		EnvItem{0, 400, 1000, 200, true, gray},
		EnvItem{300, 200, 400, 10, true, gray},
		EnvItem{250, 300, 100, 10, true, gray},
		EnvItem{650, 300, 100, 10, true, gray},
	]!
	app.cam = gg.default_camera2d()
	app.cam.offset = gg.Vec2{f32(screen_width) / 2, f32(screen_height) / 2}
	app.cam.target = app.player.position

	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 2d camera platformer'
		bg_color:     light_gray
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down || e.key_repeat {
		return
	}
	match e.key_code {
		.r {
			app.cam.zoom = 1
			app.player.position = gg.Vec2{400, 280}
		}
		.c {
			app.camera_mode = (app.camera_mode + 1) % 5
		}
		else {}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg

	if ctx.pressed_keys[int(gg.KeyCode.left)] {
		app.player.position.x -= player_horizontal_speed * dt
	}
	if ctx.pressed_keys[int(gg.KeyCode.right)] {
		app.player.position.x += player_horizontal_speed * dt
	}
	if ctx.pressed_keys[int(gg.KeyCode.space)] && app.player.can_jump {
		app.player.speed = -player_jump_speed
		app.player.can_jump = false
	}

	mut hit_obstacle := false
	for e in app.env_items {
		if !e.blocking {
			continue
		}
		p := app.player.position
		if e.x <= p.x && e.x + e.width >= p.x && e.y >= p.y
			&& e.y <= p.y + app.player.speed * dt {
			hit_obstacle = true
			app.player.speed = 0
			app.player.position.y = e.y
			break
		}
	}
	if !hit_obstacle {
		app.player.position.y += app.player.speed * dt
		app.player.speed += gravity * dt
		app.player.can_jump = false
	} else {
		app.player.can_jump = true
	}

	app.cam.zoom += f32(ctx.scroll_y) * 0.05
	if app.cam.zoom > 3 {
		app.cam.zoom = 3
	} else if app.cam.zoom < 0.25 {
		app.cam.zoom = 0.25
	}
	update_camera(mut app, dt)
}

fn update_camera(mut app App, dt f32) {
	cam := &app.cam
	pos := app.player.position
	cam.offset = gg.Vec2{f32(screen_width) / 2, f32(screen_height) / 2}

	match app.camera_mode {
		0 {
			cam.target = pos
		}
		1 {
			cam.target = pos
			mut min_x, mut min_y := f32(1000), f32(1000)
			mut max_x, mut max_y := f32(-1000), f32(-1000)
			for e in app.env_items {
				min_x = if e.x < min_x { e.x } else { min_x }
				max_x = if e.x + e.width > max_x { e.x + e.width } else { max_x }
				min_y = if e.y < min_y { e.y } else { min_y }
				max_y = if e.y + e.height > max_y { e.y + e.height } else { max_y }
			}
			max := cam.world_to_screen(gg.Vec2{max_x, max_y}, f32(screen_width), f32(screen_height))
			min := cam.world_to_screen(gg.Vec2{min_x, min_y}, f32(screen_width), f32(screen_height))
			if max.x < screen_width {
				cam.offset.x = f32(screen_width) - (max.x - f32(screen_width) / 2)
			}
			if max.y < screen_height {
				cam.offset.y = f32(screen_height) - (max.y - f32(screen_height) / 2)
			}
			if min.x > 0 {
				cam.offset.x = f32(screen_width) / 2 - min.x
			}
			if min.y > 0 {
				cam.offset.y = f32(screen_height) / 2 - min.y
			}
		}
		2 {
			diff := pos - cam.target
			length := f32(math.sqrt(f64(diff.x * diff.x + diff.y * diff.y)))
			if length > 10 {
				speed := if 0.8 * length > 30 { 0.8 * length } else { 30 }
				cam.target = cam.target + diff.scale(speed * dt / length)
			}
		}
		3 {
			cam.target.x = pos.x
			if app.evening_out {
				dir := if app.even_out_target > cam.target.y { 1 } else { -1 }
				cam.target.y += 700 * dt * dir
				if (dir > 0 && cam.target.y > app.even_out_target)
					|| (dir < 0 && cam.target.y < app.even_out_target) {
					cam.target.y = app.even_out_target
					app.evening_out = false
				}
			} else if app.player.can_jump && app.player.speed == 0
				&& pos.y != cam.target.y {
				app.evening_out = true
				app.even_out_target = pos.y
			}
		}
		4 {
			bbox_x, bbox_y := f32(0.2), f32(0.2)
			min := gg.screen_to_world(gg.Vec2{(1 - bbox_x) * 0.5 * screen_width, (1 - bbox_y) * 0.5 * screen_height}, cam, f32(screen_width), f32(screen_height))
			max := gg.screen_to_world(gg.Vec2{(1 + bbox_x) * 0.5 * screen_width, (1 + bbox_y) * 0.5 * screen_height}, cam, f32(screen_width), f32(screen_height))
			cam.offset = gg.Vec2{(1 - bbox_x) * 0.5 * screen_width, (1 - bbox_y) * 0.5 * screen_height}
			if pos.x < min.x {
				cam.target.x = pos.x
			}
			if pos.y < min.y {
				cam.target.y = pos.y
			}
			if pos.x > max.x {
				cam.target.x = min.x + (pos.x - max.x)
			}
			if pos.y > max.y {
				cam.target.y = min.y + (pos.y - max.y)
			}
		}
		else {}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.begin_2d(app.cam)
	for e in app.env_items {
		ctx.draw_rect(x: e.x, y: e.y, w: e.width, h: e.height, color: e.color)
	}
	ctx.draw_rect(x: app.player.position.x - 20, y: app.player.position.y - 40, w: 40, h: 40, color: gg.red)
	ctx.draw_circle_filled(app.player.position.x, app.player.position.y, 5, gold)
	ctx.end_2d()

	ctx.draw_text(20, 20, 'Controls:', size: 10, color: black)
	ctx.draw_text(40, 40, '- Right/Left to move', size: 10, color: dark_gray)
	ctx.draw_text(40, 60, '- Space to jump', size: 10, color: dark_gray)
	ctx.draw_text(40, 80, '- Mouse Wheel to Zoom in-out', size: 10, color: dark_gray)
	ctx.draw_text(40, 100, '- R to reset position + zoom', size: 10, color: dark_gray)
	ctx.draw_text(40, 120, '- C to change camera mode', size: 10, color: dark_gray)
	ctx.draw_text(20, 140, 'Current camera mode:', size: 10, color: black)
	descriptions := [
		'Follow player center',
		'Follow player center, but clamp to map edges',
		'Follow player center; smoothed',
		'Follow player center horizontally; update player center vertically after landing',
		'Player push camera on getting too close to screen edge',
	]
	ctx.draw_text(40, 160, descriptions[app.camera_mode], size: 10, color: dark_gray)
	ctx.end()
}
