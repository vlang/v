module main

// raylib [shapes] example - bullet hell, ported to gg.
import gg
import math

const max_bullets = 20000
const raywhite = gg.Color{245, 245, 245, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const black_c = gg.Color{0, 0, 0, 255}
const red_c = gg.Color{230, 41, 55, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const green_c = gg.Color{0, 158, 47, 255}
const purple = gg.Color{211, 22, 197, 255}
const panel = gg.Color{0, 0, 0, 200}

struct Bullet {
mut:
	x        f32
	y        f32
	ax       f32
	ay       f32
	disabled bool
	color    gg.Color
}

struct App {
mut:
	ctx             &gg.Context = unsafe { nil }
	bullets         []Bullet
	bullet_count    int
	disabled_count  int
	bullet_radius   f32 = 10
	bullet_speed    f32 = 3
	bullet_rows     int = 6
	base_direction  f32
	angle_increment int = 5
	spawn_cooldown  int = 2
	spawn_timer     int = 2
	magic_rotation  f32
	use_circles     bool = true
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	w2 := f32(ctx.width) / 2
	h2 := f32(ctx.height) / 2

	// Magic circle: rotated rectangles (DrawRectanglePro approximation via polygon).
	app.magic_rotation++
	draw_rot_square(ctx, w2, h2, 120, app.magic_rotation, purple)
	draw_rot_square(ctx, w2, h2, 120, app.magic_rotation + 45, purple)
	ctx.draw_circle_empty(w2, h2, 70, black_c)
	ctx.draw_circle_empty(w2, h2, 50, black_c)
	ctx.draw_circle_empty(w2, h2, 30, black_c)

	for i in 0 .. app.bullet_count {
		b := app.bullets[i]
		if b.disabled {
			continue
		}
		if app.use_circles {
			ctx.draw_circle_filled(b.x, b.y, app.bullet_radius, b.color)
			ctx.draw_circle_empty(b.x, b.y, app.bullet_radius, black_c)
		} else {
			// PORT NOTE: the raylib pre-rendered texture optimization is replaced by
			// a single filled+outlined circle, visually identical.
			ctx.draw_circle_filled(b.x, b.y, app.bullet_radius, b.color)
			ctx.draw_circle_empty(b.x, b.y, app.bullet_radius, black_c)
		}
	}

	ctx.draw_rect_filled(10, 10, 290, 150, panel)
	ctx.draw_text(20, 20, 'Controls:', size: 10, color: lightgray)
	ctx.draw_text(40, 40, '- Right/Left or A/D: Change rows number', size: 10, color: lightgray)
	ctx.draw_text(40, 60, '- Up/Down or W/S: Change bullet speed', size: 10, color: lightgray)
	ctx.draw_text(40, 80, '- Z or X: Change spawn cooldown', size: 10, color: lightgray)
	ctx.draw_text(40, 100, '- Space (Hold): Change the angle increment', size: 10, color: lightgray)
	ctx.draw_text(40, 120, '- Enter: Switch draw method (Performance)', size: 10, color: lightgray)
	ctx.draw_text(40, 140, '- C: Clear bullets', size: 10, color: lightgray)

	ctx.draw_rect_filled(610, 10, 175, 30, panel)
	if app.use_circles {
		ctx.draw_text(620, 20, 'Draw method: DrawTexture(*)', size: 10, color: green_c)
	} else {
		ctx.draw_text(620, 20, 'Draw method: DrawCircle(*)', size: 10, color: red_c)
	}

	active := app.bullet_count - app.disabled_count
	info := '[ Bullets: ${active}, Rows: ${app.bullet_rows}, Bullet speed: ${app.bullet_speed:.2f}, Angle increment: ${app.angle_increment}, Cooldown: ${app.spawn_cooldown} ]'
	ctx.draw_rect_filled(120, 410, 560, 30, panel)
	ctx.draw_text(135, 420, info, size: 10, color: green_c)
	ctx.end()
}

fn draw_rot_square(ctx &gg.Context, cx f32, cy f32, size f32, rot f32, c gg.Color) {
	pts := poly_points(cx, cy, size / 2, 4, rot)
	ctx.draw_convex_poly(pts, c)
}

fn poly_points(cx f32, cy f32, radius f32, edges int, rotation f32) []f32 {
	mut pts := []f32{}
	rot := f32(math.radians(rotation))
	for i in 0 .. edges {
		a := f32(math.tau) * f32(i) / f32(edges) + rot
		pts << cx + radius * math.cosf(a)
		pts << cy + radius * math.sinf(a)
	}
	return pts
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.right, .d {
			if app.bullet_rows < 359 {
				app.bullet_rows++
			}
		}
		.left, .a {
			if app.bullet_rows > 1 {
				app.bullet_rows--
			}
		}
		.up, .w { app.bullet_speed += 0.25 }
		.down, .s {
			if app.bullet_speed > 0.50 {
				app.bullet_speed -= 0.25
			}
		}
		.z {
			if app.spawn_cooldown > 1 {
				app.spawn_cooldown--
			}
		}
		.x { app.spawn_cooldown++ }
		.enter { app.use_circles = !app.use_circles }
		.c {
			app.bullet_count = 0
			app.disabled_count = 0
			// Drop stale bullets too, otherwise the old entries at indices
			// 0..bullet_count keep being drawn instead of newly spawned ones.
			app.bullets.clear()
		}
		else {}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.bullet_count >= max_bullets {
		app.bullet_count = 0
		app.disabled_count = 0
		app.bullets.clear()
	}
	app.spawn_timer--
	if app.spawn_timer < 0 {
		app.spawn_timer = app.spawn_cooldown
		degrees_per_row := 360.0 / f32(app.bullet_rows)
		for row in 0 .. app.bullet_rows {
			if app.bullet_count < max_bullets {
				dir := app.base_direction + degrees_per_row * f32(row)
				rad := f32(math.radians(dir))
				app.bullets << Bullet{
					x:     f32(app.ctx.width) / 2
					y:     f32(app.ctx.height) / 2
					ax:    app.bullet_speed * math.cosf(rad)
					ay:    app.bullet_speed * math.sinf(rad)
					color: if row % 2 == 0 { red_c } else { blue_c }
				}
				app.bullet_count++
			}
		}
		app.base_direction += f32(app.angle_increment)
	}
	lim_x := f32(app.ctx.width)
	lim_y := f32(app.ctx.height)
	for i in 0 .. app.bullet_count {
		if app.bullets[i].disabled {
			continue
		}
		app.bullets[i].x += app.bullets[i].ax
		app.bullets[i].y += app.bullets[i].ay
		b := app.bullets[i]
		if b.x < -app.bullet_radius * 2 || b.x > lim_x + app.bullet_radius * 2
			|| b.y < -app.bullet_radius * 2 || b.y > lim_y + app.bullet_radius * 2 {
			app.bullets[i].disabled = true
			app.disabled_count++
		}
	}
	if app.ctx.pressed_keys[gg.KeyCode.space] {
		app.angle_increment++
		app.angle_increment %= 360
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - bullet hell'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
