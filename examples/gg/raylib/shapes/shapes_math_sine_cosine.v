module main

// raylib [shapes] example - math sine cosine, ported to gg.
// PORT NOTE: the raygui panel is replaced by a keyboard/mouse angle control.
import gg
import math

const wave_points = 36
const raywhite = gg.Color{245, 245, 245, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const red_c = gg.Color{230, 41, 55, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const purple = gg.Color{211, 22, 197, 255}
const orange_c = gg.Color{255, 161, 0, 255}
const beige = gg.Color{211, 176, 131, 255}
const darkblue = gg.Color{0, 82, 172, 255}
const pink = gg.Color{255, 189, 196, 255}
const lime = gg.Color{1, 222, 38, 255}
const black_c = gg.Color{0, 0, 0, 255}

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	angle  f32
	paused bool
}

fn deg_angle(deg f32) f32 {
	return f32(math.radians(90 - deg))
}

fn draw_polyline(ctx &gg.Context, pts []gg.Vec2, c gg.Color) {
	for i in 1 .. pts.len {
		ctx.draw_line(pts[i].x, pts[i].y, pts[i - 1].x, pts[i - 1].y, c)
	}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	cx, cy := f32(370), f32(225)
	r := f32(130)
	arad := f32(math.radians(app.angle))
	sr, cr := math.sinf(arad), math.cosf(arad)
	px, py := cx + cr * r, cy - sr * r
	ctx.draw_circle_empty(cx, cy, r, gray_c)
	ctx.draw_line(cx, cy - r, cx, cy + r, gray_c)
	ctx.draw_line(cx - r, cy, cx + r, cy, gray_c)

	// wave graphs
	const sx = f32(20)
	const sy = f32(330)
	const sw = f32(200)
	const sh = f32(100)
	ctx.draw_line(sx, sy, sx, sy + sh, gray_c)
	ctx.draw_line(sx + sw, sy, sx + sw, sy + sh, gray_c)
	ctx.draw_line(sx, sy + sh / 2, sx + sw, sy + sh / 2, gray_c)
	mut sinepts := []gg.Vec2{cap: wave_points}
	mut cospts := []gg.Vec2{cap: wave_points}
	for i in 0 .. wave_points {
		t := f32(i) / f32(wave_points - 1)
		a := t * f32(math.tau)
		sinepts << gg.Vec2{sx + t * sw, sy + sh / 2 - math.sinf(a) * sh / 2}
		cospts << gg.Vec2{sx + t * sw, sy + sh / 2 - math.cosf(a) * sh / 2}
	}
	draw_polyline(ctx, sinepts, red_c)
	draw_polyline(ctx, cospts, blue_c)
	ctx.draw_circle_filled(sx + app.angle / 360 * sw, sy + (-sr + 1) * sh / 2, 4, red_c)
	ctx.draw_circle_filled(sx + app.angle / 360 * sw, sy + (-cr + 1) * sh / 2, 4, blue_c)

	ctx.draw_line(cx, cy, cx, py, red_c)
	ctx.draw_line(px, cy, px, py, red_c)
	ctx.draw_line(cx, cy, px, cy, blue_c)
	ctx.draw_line(cx, py, px, py, blue_c)

	tangent := math.tanf(arad)
	tx := cx + r
	ty := cy - math.max(f32(-10), math.min(f32(10), tangent)) * r
	ctx.draw_line(cx + r, cy, cx + r, ty, purple)
	ctx.draw_line(cx, cy, tx, ty, purple)

	ctx.draw_slice_empty(cx, cy, r * 0.6, deg_angle(-app.angle), deg_angle(-90), 36, beige)
	ctx.draw_slice_empty(cx, cy, r * 0.5, deg_angle(-app.angle), deg_angle(-180), 36, darkblue)
	ctx.draw_slice_empty(cx, cy, r * 0.4, deg_angle(-app.angle), deg_angle(-360), 36, pink)
	ctx.draw_slice_empty(cx, cy, r * 0.7, deg_angle(-app.angle), deg_angle(0), 36, lime)
	ctx.draw_line(cx, cy, px, py, black_c)
	ctx.draw_circle_filled(px, py, 4, black_c)

	ctx.draw_text(640, 40, 'Angle ${int(app.angle)}° [SPACE] pause', size: 12, color: gray_c)
	ctx.draw_text(640, 190, 'Sine ${sr:.2f}', size: 12, color: red_c)
	ctx.draw_text(640, 210, 'Cosine ${cr:.2f}', size: 12, color: blue_c)
	ctx.draw_text(640, 230, 'Tangent ${tangent:.2f}', size: 12, color: purple)
	ctx.draw_text(640, 250, 'Cotangent ${1 / tangent:.2f}', size: 12, color: orange_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.paused = !app.paused
	}
	if e.typ == .mouse_move && app.ctx.mouse_buttons.has(.left) && e.mouse_x > 640 && e.mouse_x < 760 && e.mouse_y > 40 && e.mouse_y < 60 {
		app.angle = (e.mouse_x - 640) / 120 * 360
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if !app.paused {
		app.angle++
		if app.angle >= 360 { app.angle -= 360 }
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - math sine cosine'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
