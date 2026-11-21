module main

// raylib [shapes] example - double pendulum, ported to gg.
import gg
import math

const simulation_steps = 30
const g_grav = f32(9.81)
// PORT NOTE: gg has no offscreen render target in the legacy 2D API. A translucent
// background color makes gg's per-frame fade quad produce a similar motion trail.
const trail_bg = gg.Color{0, 0, 0, 6}
const red_c = gg.Color{230, 41, 55, 255}
const raywhite = gg.Color{245, 245, 245, 255}

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	l1     f32         = 15
	m1     f32         = 0.2
	t1     f32         = f32(math.radians(170))
	w1     f32
	l2     f32 = 15
	m2     f32 = 0.1
	t2     f32
	w2     f32
	prev_x f32
	prev_y f32
	curr_x f32
	curr_y f32
}

fn endpoint(l f32, theta f32) (f32, f32) {
	return 10 * l * math.sinf(theta), 10 * l * math.cosf(theta)
}

fn double_endpoint(l1 f32, t1 f32, l2 f32, t2 f32) (f32, f32) {
	x1, y1 := endpoint(l1, t1)
	x2, y2 := endpoint(l2, t2)
	return x1 + x2, y1 + y2
}

fn draw_rot_bar(ctx &gg.Context, x f32, y f32, len f32, thick f32, angle f32, c gg.Color) {
	rad := f32(math.radians(angle))
	ux, uy := math.cosf(rad), math.sinf(rad)
	px, py := -uy * thick * 0.5, ux * thick * 0.5
	ctx.draw_convex_poly([
		x + px,
		y + py,
		x + ux * len + px,
		y + uy * len + py,
		x + ux * len - px,
		y + uy * len - py,
		x - px,
		y - py,
	], c)
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	// Trail: the translucent background fades the previous frame's marks.
	ctx.draw_circle_filled(app.curr_x, app.curr_y, 2, red_c)
	ctx.draw_line(app.prev_x, app.prev_y, app.curr_x, app.curr_y, red_c)

	// Pendulum arms
	draw_rot_bar(ctx, f32(ctx.width) / 2, f32(ctx.height) / 2 - 100, 10 * app.l1, 20,
		90 - f32(math.degrees(app.t1)), raywhite)
	x1, y1 := endpoint(app.l1, app.t1)
	draw_rot_bar(ctx, f32(ctx.width) / 2 + x1, f32(ctx.height) / 2 - 100 + y1, 10 * app.l2, 20,
		90 - f32(math.degrees(app.t2)), raywhite)
	ctx.end()
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	length_scaler := f32(0.1)
	len1 := app.l1 * length_scaler
	len2 := app.l2 * length_scaler
	total_mass := app.m1 + app.m2
	step := dt / simulation_steps
	step2 := step * step
	for _ in 0 .. simulation_steps {
		delta := app.t1 - app.t2
		sin_d := math.sinf(delta)
		cos_d := math.cosf(delta)
		cos_2d := math.cosf(2 * delta)
		ww1, ww2 := app.w1 * app.w1, app.w2 * app.w2
		den := len1 * (2 * app.m1 + app.m2 - app.m2 * cos_2d)
		if den == 0 {
			continue
		}
		a1 := (-g_grav * (2 * app.m1 + app.m2) * math.sinf(app.t1) -
			app.m2 * g_grav * math.sinf(app.t1 - 2 * app.t2) -
			2 * sin_d * app.m2 * (ww2 * len2 + ww1 * len1 * cos_d)) / den
		a2 := (2 * sin_d * (ww1 * len1 * total_mass + g_grav * total_mass * math.cosf(app.t1) +
			ww2 * len2 * app.m2 * cos_d)) / (len2 * (2 * app.m1 + app.m2 - app.m2 * cos_2d))
		app.t1 += app.w1 * step + 0.5 * a1 * step2
		app.t2 += app.w2 * step + 0.5 * a2 * step2
		app.w1 += a1 * step
		app.w2 += a2 * step
	}
	x, y := double_endpoint(app.l1, app.t1, app.l2, app.t2)
	app.prev_x = app.curr_x
	app.prev_y = app.curr_y
	app.curr_x = x + f32(app.ctx.width) / 2
	app.curr_y = y + f32(app.ctx.height) / 2 - 100
}

fn main() {
	mut app := &App{}
	x, y := double_endpoint(app.l1, app.t1, app.l2, app.t2)
	app.curr_x = x + 400
	app.curr_y = y + 125
	app.prev_x = app.curr_x
	app.prev_y = app.curr_y
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - double pendulum'
		user_data:    app
		frame_fn:     on_frame
		update_fn:    on_update
		bg_color:     trail_bg
	)
	app.ctx.run()
}
