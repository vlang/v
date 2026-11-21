module main

// raylib [shapes] example - ring drawing, ported to gg.
// PORT NOTE: raygui controls replaced by keyboard/mouse control.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const maroon_t = gg.Color{190, 33, 55, 90}
const black_t = gg.Color{0, 0, 0, 102}
const maroon = gg.Color{190, 33, 55, 255}
const darkgray = gg.Color{80, 80, 80, 255}

fn da(deg f32) f32 { return f32(math.radians(90 - deg)) }

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	inner    f32         = 80
	outer    f32         = 190
	start    f32
	end      f32 = 360
	segments f32
	fill     bool = true
	lines    bool
	circle   bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_line(500, 0, 500, f32(ctx.height), gg.Color{200, 200, 200, 150})
	ctx.draw_rect_filled(500, 0, f32(ctx.width - 500), f32(ctx.height), gg.Color{200, 200, 200, 80})
	cx, cy := f32(ctx.width - 300) / 2, f32(ctx.height) / 2
	seg := int(app.segments)
	if app.fill {
		ctx.draw_arc_filled(cx, cy, app.inner, app.outer - app.inner, da(app.start), da(app.end), seg, maroon_t)
	}
	if app.lines {
		ctx.draw_arc_empty(cx, cy, app.inner, app.outer - app.inner, da(app.start), da(app.end), seg, black_t)
	}
	if app.circle {
		ctx.draw_slice_empty(cx, cy, app.outer, da(app.start), da(app.end), seg, black_t)
	}
	ctx.draw_text(600, 20, 'Start ${int(app.start)} / End ${int(app.end)} [Q/W/E/R]',
		size:  12
		color: darkgray
	)
	ctx.draw_text(600, 60, 'Inner ${int(app.inner)} / Outer ${int(app.outer)} [A/S/D/F]',
		size:  12
		color: darkgray
	)
	ctx.draw_text(600, 100, 'Segments ${int(app.segments)} [Z/X]', size: 12, color: darkgray)
	ctx.draw_text(600, 140, 'Fill:${app.fill} Lines:${app.lines} Circle:${app.circle} [1/2/3]',
		size:  12
		color: darkgray
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down { return }
	match e.key_code {
		.q { app.start -= 15 }
		.w { app.start += 15 }
		.e { app.end -= 15 }
		.r { app.end += 15 }
		.a {
			if app.inner > 0 { app.inner -= 10 }
		}
		.s { app.inner += 10 }
		.d {
			if app.outer > 0 { app.outer -= 10 }
		}
		.f { app.outer += 10 }
		.z {
			if app.segments > 0 { app.segments -= 1 }
		}
		.x { app.segments += 1 }
		._1 { app.fill = !app.fill }
		._2 { app.lines = !app.lines }
		._3 { app.circle = !app.circle }
		else {}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - ring drawing'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
