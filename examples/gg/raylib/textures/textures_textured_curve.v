module main

// Port of raylib textures_textured_curve.c
// PORT NOTE: gg has no immediate-mode textured quads (rlgl). Each bezier
// segment is drawn as a rotated, stretched piece of the road texture, which
// reproduces the ribbon without exposing raw vertex/texcoord calls.
import gg
import math

const curve_raywhite = gg.Color{245, 245, 245, 255}
const curve_darkgray = gg.Color{80, 80, 80, 255}
const curve_blue = gg.Color{0, 121, 241, 255}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	road       gg.Image
	show_curve bool
	width      f32 = 50
	segments   int = 24
	start      gg.Rect
	start_tan  gg.Rect
	end        gg.Rect
	end_tan    gg.Rect
	selected   int = -1
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.road = app.ctx.create_image('resources/road.png') or { gg.Image{} }
	app.start = gg.Rect{80, 100, 0, 0}
	app.start_tan = gg.Rect{100, 300, 0, 0}
	app.end = gg.Rect{700, 350, 0, 0}
	app.end_tan = gg.Rect{600, 100, 0, 0}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()

	step := 1.0 / f32(app.segments)
	mut px := app.start.x
	mut py := app.start.y
	mut prev_tan_x := f32(0)
	mut prev_tan_y := f32(0)
	for i := 1; i <= app.segments; i++ {
		t := step * f32(i)
		a := f32(math.pow(f64(1 - t), 3))
		b := f32(3.0 * math.pow(f64(1 - t), 2) * f64(t))
		c := f32(3.0 * (1 - t) * math.pow(f64(t), 2))
		d := f32(math.pow(f64(t), 3))
		cx := a * app.start.x + b * app.start_tan.x + c * app.end_tan.x + d * app.end.x
		cy := a * app.start.y + b * app.start_tan.y + c * app.end_tan.y + d * app.end.y
		dx := cx - px
		dy := cy - py
		len := f32(math.sqrt(f64(dx * dx + dy * dy)))
		mut nx := f32(0)
		mut ny := f32(0)
		if len > 0 {
			nx = -dy / len
			ny = dx / len
		}
		if i == 1 {
			prev_tan_x = nx
			prev_tan_y = ny
		}
		angle := f32(math.atan2(f64(dy), f64(dx))) * 180.0 / f32(math.pi)
		mid_x := (px + cx) / 2
		mid_y := (py + cy) / 2
		app.ctx.draw_image_with_config(
			img:       &app.road
			img_rect:  gg.Rect{mid_x - len / 2, mid_y - app.width, len, app.width * 2}
			part_rect: gg.Rect{0, 0, f32(app.road.width), f32(app.road.height)}
			rotation:  angle
		)
		_ = prev_tan_x
		_ = prev_tan_y
		px = cx
		py = cy
	}

	if app.show_curve {
		app.ctx.draw_cubic_bezier([app.start.x, app.start.y, app.start_tan.x, app.start_tan.y,
			app.end_tan.x, app.end_tan.y, app.end.x, app.end.y], curve_blue)
	}
	app.ctx.draw_line(app.start.x, app.start.y, app.start_tan.x, app.start_tan.y, gg.Color{135, 206, 235, 255})
	app.ctx.draw_line(app.start_tan.x, app.start_tan.y, app.end_tan.x, app.end_tan.y,
		gg.Color{211, 211, 211, 102})
	app.ctx.draw_line(app.end.x, app.end.y, app.end_tan.x, app.end_tan.y, gg.Color{128, 0, 128, 255})
	app.ctx.draw_circle_filled(app.start.x, app.start.y, 5, gg.red)
	app.ctx.draw_circle_filled(app.start_tan.x, app.start_tan.y, 5, gg.Color{128, 0, 0, 255})
	app.ctx.draw_circle_filled(app.end.x, app.end.y, 5, gg.green)
	app.ctx.draw_circle_filled(app.end_tan.x, app.end_tan.y, 5, gg.Color{0, 100, 0, 255})
	app.ctx.draw_text(10, 10, 'Drag points to move curve, press SPACE to show/hide base curve',
		size:  10
		color: curve_darkgray
	)
	app.ctx.draw_text(10, 30, 'Curve width: ${app.width:.0f} (Use + and - to adjust)',
		size:  10
		color: curve_darkgray
	)
	app.ctx.draw_text(10, 50, 'Curve segments: ${app.segments} (Use LEFT and RIGHT to adjust)',
		size:  10
		color: curve_darkgray
	)
	app.ctx.end()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if !app.ctx.mouse_buttons.has(.left) {
		app.selected = -1
		return
	}
	mx := f32(app.ctx.mouse_pos_x)
	my := f32(app.ctx.mouse_pos_y)
	if app.selected < 0 {
		pts := [app.start, app.start_tan, app.end, app.end_tan]
		for i in 0 .. pts.len {
			p := pts[i]
			dx := mx - p.x
			dy := my - p.y
			if dx * dx + dy * dy < 36 {
				app.selected = i
				break
			}
		}
	}
	if app.selected >= 0 {
		dx := f32(app.ctx.mouse_dx)
		dy := f32(app.ctx.mouse_dy)
		match app.selected {
			0 {
				app.start.x += dx
				app.start.y += dy
			}
			1 {
				app.start_tan.x += dx
				app.start_tan.y += dy
			}
			2 {
				app.end.x += dx
				app.end.y += dy
			}
			else {
				app.end_tan.x += dx
				app.end_tan.y += dy
			}
		}
	}
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.space { app.show_curve = !app.show_curve }
		.equal { app.width += 2 }
		.minus {
			app.width -= 2
			if app.width < 2 { app.width = 2 }
		}
		.left {
			app.segments -= 2
			if app.segments < 2 { app.segments = 2 }
		}
		.right { app.segments += 2 }
		else {}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - textured curve'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		update_fn:    update
		bg_color:     curve_raywhite
	)
	app.ctx.run()
}
