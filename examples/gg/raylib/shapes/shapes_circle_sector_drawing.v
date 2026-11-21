module main

// raylib [shapes] example - circle sector drawing, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const maroon = gg.Color{190, 33, 55, 255}
const maroon_t = gg.Color{190, 33, 55, 90}
const maroon_t2 = gg.Color{190, 33, 55, 160}
const light_t = gg.Color{200, 200, 200, 150}
const light_t2 = gg.Color{200, 200, 200, 80}

struct App {
mut:
	ctx         &gg.Context = unsafe { nil }
	start_angle f32
	end_angle   f32 = 180
	radius      f32 = 180
	segments    f32 = 10
	drag        int = -1
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	cx := f32(ctx.width - 300) / 2
	cy := f32(ctx.height) / 2

	ctx.draw_line(500, 0, 500, f32(ctx.height), light_t)
	ctx.draw_rect_filled(500, 0, f32(ctx.width - 500), f32(ctx.height), light_t2)

	// PORT NOTE: raylib angles are degrees clockwise from +x; gg slice angles are
	// radians clockwise from +y, so convert with radians(90 - angle).
	sa := f32(math.radians(90 - app.start_angle))
	ea := f32(math.radians(90 - app.end_angle))
	seg := int(app.segments)
	ctx.draw_slice_filled(cx, cy, app.radius, sa, ea, seg, maroon_t)
	ctx.draw_slice_empty(cx, cy, app.radius, sa, ea, seg, maroon_t2)

	// Lightweight sliders (raygui is not available in gg).
	draw_slider(ctx, 600, 40, app.start_angle, 0, 720, 'StartAngle')
	draw_slider(ctx, 600, 70, app.end_angle, 0, 720, 'EndAngle')
	draw_slider(ctx, 600, 140, app.radius, 0, 200, 'Radius')
	draw_slider(ctx, 600, 170, app.segments, 0, 100, 'Segments')

	min_segments := math.ceil((app.end_angle - app.start_angle) / 90)
	mode := if app.segments >= min_segments { 'MANUAL' } else { 'AUTO' }
	mode_color := if app.segments >= min_segments { maroon } else { darkgray }
	ctx.draw_text(600, 200, 'MODE: ${mode}', size: 10, color: mode_color)
	ctx.end()
}

fn draw_slider(ctx &gg.Context, x int, y int, value f32, min f32, max f32, label string) {
	ctx.draw_rect_filled(x, y, 120, 20, lightgray)
	pos := int((value - min) / (max - min) * 120)
	ctx.draw_rect_filled(x + pos - 2, y, 4, 20, maroon)
	ctx.draw_text(x, y - 14, label, size: 10, color: darkgray)
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	mx, my := f32(e.mouse_x), f32(e.mouse_y)
	match e.typ {
		.mouse_down {
			if mx >= 600 && mx <= 720 {
				if my >= 40 && my <= 60 {
					app.drag = 0
				} else if my >= 70 && my <= 90 {
					app.drag = 1
				} else if my >= 140 && my <= 160 {
					app.drag = 2
				} else if my >= 170 && my <= 190 {
					app.drag = 3
				}
			}
		}
		.mouse_up {
			app.drag = -1
		}
		.mouse_move {
			if app.drag >= 0 && mx >= 600 && mx <= 720 {
				v := (mx - 600) / 120
				match app.drag {
					0 { app.start_angle = v * 720 }
					1 { app.end_angle = v * 720 }
					2 { app.radius = v * 200 }
					3 { app.segments = v * 100 }
					else {}
				}
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
		window_title: 'raylib [shapes] example - circle sector drawing'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
