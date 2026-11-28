module main

// raylib [shapes] example - rounded rectangle drawing, ported to gg.
// PORT NOTE: raygui sliders replaced by keyboard controls; gg's rounded rect
// ignores a segment count, so the segments value is kept as a visual control only.
import gg

const (
	raywhite  = gg.Color{245, 245, 245, 255}
	light_t   = gg.Color{200, 200, 200, 150}
	light_t2  = gg.Color{200, 200, 200, 80}
	gold_t    = gg.Color{255, 203, 0, 153}
	maroon_t  = gg.Color{190, 33, 55, 51}
	maroon_t2 = gg.Color{190, 33, 55, 102}
	maroon    = gg.Color{190, 33, 55, 255}
	darkgray  = gg.Color{80, 80, 80, 255}
)

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	roundness f32 = 0.2
	width     f32 = 200
	height    f32 = 100
	segments  f32
	thick     f32 = 1
	fill      bool = true
	lines     bool
	rect      bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_line(560, 0, 560, f32(ctx.height), light_t)
	ctx.draw_rect_filled(560, 0, f32(ctx.width - 560), f32(ctx.height), light_t2)
	rx := (f32(ctx.width) - app.width - 250) / 2
	ry := (f32(ctx.height) - app.height) / 2
	if app.rect { ctx.draw_rect_filled(rx, ry, app.width, app.height, gold_t) }
	radius := app.roundness * (if app.width < app.height { app.width } else { app.height }) / 2
	if app.fill {
		ctx.draw_rounded_rect_filled(rx, ry, app.width, app.height, radius, maroon_t)
	}
	if app.lines {
		for i in 0 .. int(app.thick) {
			o := f32(i)
			ctx.draw_rounded_rect_empty(rx - o, ry - o, app.width + o * 2, app.height + o * 2, radius, maroon_t2)
		}
	}
	ctx.draw_text(640, 20, 'Width ${int(app.width)} [A/Z]', size: 12, color: darkgray)
	ctx.draw_text(640, 50, 'Height ${int(app.height)} [S/X]', size: 12, color: darkgray)
	ctx.draw_text(640, 80, 'Roundness ${app.roundness:.2f} [D/C]', size: 12, color: darkgray)
	ctx.draw_text(640, 110, 'Thickness ${int(app.thick)} [F/V]', size: 12, color: darkgray)
	ctx.draw_text(640, 140, 'Segments ${int(app.segments)} [G/B]', size: 12, color: darkgray)
	ctx.draw_text(640, 170, 'Rounded:${app.fill} Lines:${app.lines} Rect:${app.rect} [1/2/3]',
		size:  12
		color: darkgray
	)
	mode := if app.segments >= 4 { 'MANUAL' } else { 'AUTO' }
	mode_c := if app.segments >= 4 { maroon } else { darkgray }
	ctx.draw_text(640, 200, 'MODE: ${mode}', size: 10, color: mode_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down { return }
	match e.key_code {
		.a { app.width += 10 }
		.z { app.width -= 10 }
		.s { app.height += 10 }
		.x { app.height -= 10 }
		.d { if app.roundness < 1 { app.roundness += 0.05 } }
		.c { if app.roundness > 0 { app.roundness -= 0.05 } }
		.f { app.thick++ }
		.v { if app.thick > 1 { app.thick-- } }
		.g { app.segments += 2 }
		.b { if app.segments > 0 { app.segments -= 2 } }
		._1 { app.fill = !app.fill }
		._2 { app.lines = !app.lines }
		._3 { app.rect = !app.rect }
		else {}
	}
	if app.width < 0 { app.width = 0 }
	if app.height < 0 { app.height = 0 }
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - rounded rectangle drawing'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
