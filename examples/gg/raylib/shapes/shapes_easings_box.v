module main

// raylib [shapes] example - easings box, ported to gg.
import gg
import math
import math.easing

const raywhite = gg.Color{245, 245, 245, 255}
const black_t = gg.Color{0, 0, 0, 255}
const lightgray = gg.Color{200, 200, 200, 255}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	x        f32         = 400
	y        f32         = -100
	w        f32         = 100
	h        f32         = 100
	rotation f32
	alpha    f32 = 1
	state    int
	frames   int
}

fn draw_rot_rect(ctx &gg.Context, x f32, y f32, w f32, h f32, angle f32, c gg.Color) {
	rad := f32(math.radians(angle))
	ux, uy := math.cosf(rad), math.sinf(rad)
	pts := [
		x - ux * w / 2 - uy * h / 2,
		y - uy * w / 2 + ux * h / 2,
		x + ux * w / 2 - uy * h / 2,
		y + uy * w / 2 + ux * h / 2,
		x + ux * w / 2 + uy * h / 2,
		y + uy * w / 2 - ux * h / 2,
		x - ux * w / 2 + uy * h / 2,
		y - uy * w / 2 - ux * h / 2,
	]
	ctx.draw_convex_poly(pts, c)
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	mut c := black_t
	c.a = u8(255 * math.max(f64(0), f64(app.alpha)))
	draw_rot_rect(ctx, app.x, app.y, app.w, app.h, app.rotation, c)
	ctx.draw_text(10, ctx.height - 25, 'PRESS [SPACE] TO RESET BOX ANIMATION!',
		size:  20
		color: lightgray
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.x = 400
		app.y = -100
		app.w = 100
		app.h = 100
		app.rotation = 0
		app.alpha = 1
		app.state = 0
		app.frames = 0
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	app.frames++
	f := f64(app.frames)
	match app.state {
		0 {
			app.y = -100 + f32(easing.out_elastic(f / 120.0)) * 325
			if app.frames >= 120 {
				app.frames = 0
				app.state = 1
			}
		}
		1 {
			app.h = 100 + f32(easing.out_bounce(f / 120.0)) * -90
			app.w = 100 + f32(easing.out_bounce(f / 120.0)) * 700
			if app.frames >= 120 {
				app.frames = 0
				app.state = 2
			}
		}
		2 {
			app.rotation = f32(easing.out_quad(f / 240.0)) * 270
			if app.frames >= 240 {
				app.frames = 0
				app.state = 3
			}
		}
		3 {
			app.h = 10 + f32(easing.out_circ(f / 120.0)) * 790
			if app.frames >= 120 {
				app.frames = 0
				app.state = 4
			}
		}
		4 {
			app.alpha = 1 + f32(easing.out_sine(f / 160.0)) * -2
			if app.frames >= 160 {
				app.frames = 0
				app.state = 5
			}
		}
		else {}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - easings box'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
