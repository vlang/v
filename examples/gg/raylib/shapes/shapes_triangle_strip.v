module main

// raylib [shapes] example - triangle strip, ported to gg.
// PORT NOTE: raygui controls replaced by keyboard controls.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}

fn hsv(h f32) gg.Color {
	c, x, m := f32(1), f32(1 - math.absf((h / 60) % 2 - 1)), f32(0)
	r, g, b := f32(0), f32(0), f32(0)
	if h < 60 {
		r, g = c, x
	} else if h < 120 {
		r, g = x, c
	} else if h < 180 {
		g, b = c, x
	} else if h < 240 {
		g, b = x, c
	} else if h < 300 {
		r, b = x, c
	} else {
		r, b = c, x
	}
	return gg.Color{u8((r + m) * 255), u8((g + m) * 255), u8((b + m) * 255), 255}
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	segs    int         = 6
	inner   f32         = 100
	outer   f32         = 150
	outline bool        = true
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	cx, cy := f32(275), f32(225)
	step := f32(math.tau) / f32(app.segs)
	for i in 0 .. app.segs {
		a1 := f32(i) * step
		a2 := a1 + step / 2
		a := gg.Vec2{cx + math.cosf(a1) * app.inner, cy + math.sinf(a1) * app.inner}
		b := gg.Vec2{cx + math.cosf(a2) * app.outer, cy + math.sinf(a2) * app.outer}
		na1 := a1 + step
		c := gg.Vec2{cx + math.cosf(na1) * app.inner, cy + math.sinf(na1) * app.inner}
		d := gg.Vec2{cx + math.cosf(a1 + step * 1.5) * app.outer, cy + math.sinf(a1 + step * 1.5) * app.outer}
		ctx.draw_triangle_filled(c.x, c.y, b.x, b.y, a.x, a.y, hsv(f32(math.degrees(a1))))
		ctx.draw_triangle_filled(d.x, d.y, b.x, b.y, c.x, c.y, hsv(f32(math.degrees(a1 + step / 2))))
		if app.outline {
			ctx.draw_triangle_empty(a.x, a.y, b.x, b.y, c.x, c.y, black_c)
			ctx.draw_triangle_empty(c.x, c.y, b.x, b.y, d.x, d.y, black_c)
		}
	}
	ctx.draw_text(640, 30, 'Segments ${app.segs} [+/-]', size: 16, color: black_c)
	ctx.draw_text(640, 60, 'Outline ${app.outline} [O]', size: 16, color: black_c)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down { return }
	match e.key_code {
		.equal, .kp_add {
			if app.segs < 60 { app.segs++ }
		}
		.minus, .kp_subtract {
			if app.segs > 6 { app.segs-- }
		}
		.o { app.outline = !app.outline }
		else {}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - triangle strip'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
