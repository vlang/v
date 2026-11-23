module main

// raylib [games] classic - floppy, ported to gg.
// A flappy-bird style game: tap SPACE to lift the floppy through pairs of
// pipes. Scoring a pipe adds 100 points; touching a pipe ends the run.
// P pauses, ENTER starts a new run after a crash.

import gg
import rand

const screen_width = 800
const screen_height = 450

const max_tubes = 100
const floppy_radius = f32(24)
const tubes_width = f32(80)
const tubes_height = f32(255)
const tubes_speed_x = f32(2)

const ray_white = gg.Color{245, 245, 245, 255}
const white = gg.Color{255, 255, 255, 255}
const gray = gg.Color{130, 130, 130, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const light_gray = gg.Color{200, 200, 200, 255}

struct Floppy {
mut:
	position gg.Vec2
	radius   f32
}

struct Tube {
mut:
	// position holds the x of the pipe pair and the y of its top segment.
	position gg.Vec2
	active   bool
}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	floppy    Floppy
	tubes     []Tube
	game_over bool
	paused    bool
	score     int
	hi_score  int
	superfx   bool
}

fn init_game(mut app &App) {
	app.floppy.radius = floppy_radius
	app.floppy.position = gg.Vec2{80, f32(screen_height) / 2 - floppy_radius}

	app.tubes = []Tube{len: max_tubes}
	for i in 0 .. max_tubes {
		app.tubes[i].position = gg.Vec2{f32(400 + 280 * i), f32(-(rand.intn(121) or { 0 }))}
		app.tubes[i].active = true
	}

	app.score = 0
	app.game_over = false
	app.paused = false
	app.superfx = false
}

// circle_rec_collision is the gg counterpart of raylib's CheckCollisionCircleRec.
// The closest point on the rectangle is used so corners are handled properly.
fn circle_rec_collision(center gg.Vec2, radius f32, x f32, y f32, w f32, h f32) bool {
	mut closest_x := center.x
	if closest_x < x {
		closest_x = x
	}
	if closest_x > x + w {
		closest_x = x + w
	}
	mut closest_y := center.y
	if closest_y < y {
		closest_y = y
	}
	if closest_y > y + h {
		closest_y = y + h
	}
	dx := center.x - closest_x
	dy := center.y - closest_y
	return dx * dx + dy * dy <= radius * radius
}

fn fmt_score(n int) string {
	s := n.str()
	if s.len >= 4 {
		return s
	}
	return '0000'[0..4 - s.len] + s
}

fn update(mut app &App) {
	if app.game_over || app.paused {
		return
	}

	for i in 0 .. max_tubes {
		mut t := app.tubes[i]
		t.position.x -= tubes_speed_x
		app.tubes[i] = t
	}

	ctx := app.ctx
	if ctx.pressed_keys[gg.KeyCode.space] {
		app.floppy.position.y -= 3
	} else {
		app.floppy.position.y += 1
	}

	for i in 0 .. max_tubes {
		t := app.tubes[i]
		tx := t.position.x
		ty := t.position.y
		// The lower pipe is offset so a 90px gap is left below the upper pipe.
		bottom_y := ty + 600 - tubes_height
		if circle_rec_collision(app.floppy.position, app.floppy.radius, tx, ty,
			tubes_width, tubes_height) || circle_rec_collision(app.floppy.position,
			app.floppy.radius, tx, bottom_y, tubes_width, tubes_height) {
			app.game_over = true
			app.paused = false
			continue
		}
		if tx < app.floppy.position.x && t.active && !app.game_over {
			app.tubes[i].active = false
			app.score += 100
			app.superfx = true
			if app.score > app.hi_score {
				app.hi_score = app.score
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
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	if !app.game_over {
		ctx.draw_circle_filled(app.floppy.position.x, app.floppy.position.y,
			app.floppy.radius, dark_gray)

		for i in 0 .. max_tubes {
			t := app.tubes[i]
			ctx.draw_rect_filled(t.position.x, t.position.y, tubes_width, tubes_height, gray)
			ctx.draw_rect_filled(t.position.x, t.position.y + 600 - tubes_height,
				tubes_width, tubes_height, gray)
		}

		// One-frame white flash when a pipe is cleared.
		if app.superfx {
			ctx.draw_rect_filled(0, 0, screen_width, screen_height, white)
			app.superfx = false
		}

		ctx.draw_text(20, 20, fmt_score(app.score), size: 40, color: gray)
		ctx.draw_text(20, 70, 'HI-SCORE: ${fmt_score(app.hi_score)}', size: 20, color: light_gray)
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
		window_title: 'classic game: floppy'
		bg_color:     ray_white
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
	)
	app.ctx.run()
}
