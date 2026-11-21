module main

// raylib [text] example - 3d drawing, ported to gg.
// PORT NOTE: gg's legacy 2D API has no per-glyph 3D quads, font atlas UV access,
// alpha-discard shader, or 3D camera text. This port reproduces the scene in 2D
// with projected geometry: a wire cube, a grid, layered waving text and the
// interactive size/spacing/layer controls. The 3D glyph rendering itself is not
// representable with gg's 2D primitives.
import gg
import math
import rand

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const red_c = gg.Color{230, 41, 55, 255}
const maroon = gg.Color{190, 33, 55, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const darkblue = gg.Color{0, 82, 172, 255}
const darkgreen = gg.Color{0, 117, 44, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const violet = gg.Color{135, 61, 190, 255}
const max_layers = 32

fn hsv(h f32) gg.Color {
	c, x, m := f32(1), f32(1 - math.abs(f32(math.fmod(f64(h / 60), 2.0)) - 1)), f32(0)
	mut r, mut g, mut b := f32(0), f32(0), f32(0)
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
	ctx         &gg.Context = unsafe { nil }
	text        string      = 'Hello ~~World~~ in 3D!'
	font_size   f32         = 20
	spacing     f32         = 2
	line_gap    f32         = 4
	layers      int         = 1
	layer_gap   f32         = 4
	spin        bool        = true
	multicolor  bool
	show_letter bool
	show_text   bool
	time        f32
	colors      []gg.Color
}

fn (mut app App) randomize_colors() {
	app.colors = []gg.Color{len: max_layers}
	for i in 0 .. max_layers {
		app.colors[i] = hsv(f32(rand.intn(360) or { 0 }))
	}
}

fn draw_grid(ctx &gg.Context) {
	for i in 0 .. 20 {
		x := f32(i) * 40
		ctx.draw_line(x, 0, x, f32(ctx.height), gg.Color{200, 200, 200, 255})
	}
	for i in 0 .. 12 {
		y := f32(i) * 40
		ctx.draw_line(0, y, f32(ctx.width), y, gg.Color{200, 200, 200, 255})
	}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	draw_grid(ctx)
	// Wire cube (isometric projection)
	cx, cy := f32(400), f32(190)
	ctx.draw_rect_empty(cx - 60, cy - 60, 120, 120, maroon)
	ctx.draw_rect_empty(cx - 45, cy - 75, 120, 120, maroon)
	ctx.draw_line(cx - 60, cy - 60, cx - 45, cy - 75, maroon)
	ctx.draw_line(cx + 60, cy - 60, cx + 75, cy - 75, maroon)
	ctx.draw_line(cx - 60, cy + 60, cx - 45, cy + 75, maroon)
	ctx.draw_line(cx + 60, cy + 60, cx + 75, cy + 75, maroon)

	// Layered waving text
	mut wave := false
	mut xpos := f32(160)
	mut k := 0
	for i := 0; i < app.text.len; i++ {
		ch := app.text[i..i + 1]
		if ch == '~' && i + 1 < app.text.len && app.text[i + 1] == `~` {
			wave = !wave
			i++
			continue
		}
		mut dx := f32(0)
		mut dy := f32(0)
		if wave && app.spin {
			dx = math.sinf(app.time * 3 - f32(k) * 0.35) * 9
			dy = math.sinf(app.time * 3 - f32(k) * 0.35) * 7
		}
		for l in 0 .. app.layers {
			c := if app.multicolor { app.colors[l] } else { maroon }
			ctx.draw_text(int(xpos + dx), int(120 + f32(l) * app.layer_gap + dy), ch,
				size:  int(app.font_size)
				color: c
			)
		}
		xpos += f32(ctx.text_width(ch)) + app.spacing
		k++
	}
	ctx.draw_text(10, 35,
		'Drag & drop a font file to change the font!\nType something, see what happens!\n\nPress [F3] to toggle the camera',
		size:  10
		color: black_c
	)
	ctx.draw_text(10, 355, 'F1 letters  F2 bounds  F3 spin  Home/End layers  Arrows size/spacing',
		size:  12
		color: darkgray
	)
	ctx.draw_text(10, 380,
		'[Tab] multicolor  click cube: random color  Backspace: delete  Enter: newline',
		size:  12
		color: darkgray
	)
	ctx.draw_text(ctx.width - 300, 10,
		'${app.layers} layer(s) - ${if app.spin { 'ORBITAL' } else { 'FREE' }} camera',
		size:  12
		color: darkgreen
	)
	ctx.draw_text(10, ctx.height - 16, 'PORT NOTE: 2D projection of the original 3D glyph quads',
		size:  10
		color: violet
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down { return }
	match e.key_code {
		.f1 { app.show_letter = !app.show_letter }
		.f2 { app.show_text = !app.show_text }
		.f3 { app.spin = !app.spin }
		.home {
			if app.layers > 1 { app.layers-- }
		}
		.end {
			if app.layers < max_layers { app.layers++ }
		}
		.left { app.font_size -= 1 }
		.right { app.font_size += 1 }
		.up { app.spacing -= 1 }
		.down { app.spacing += 1 }
		.page_up { app.line_gap -= 1 }
		.page_down { app.line_gap += 1 }
		.tab { app.multicolor = !app.multicolor }
		.backspace {
			if app.text.len > 0 { app.text = app.text[..app.text.len - 1] }
		}
		.enter { app.text += '\n' }
		else {}
	}
	if app.font_size < 6 { app.font_size = 6 }
	if e.typ == .char && app.text.len < 60 {
		app.text += '${rune(e.char_code)}'
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.time += dt
}

fn main() {
	mut app := &App{}
	app.randomize_colors()
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - 3d drawing'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
