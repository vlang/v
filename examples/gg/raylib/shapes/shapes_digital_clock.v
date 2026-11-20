module main

// raylib [shapes] example - digital clock, ported to gg.
import gg
import math
import time

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const lightgray_t = gg.Color{200, 200, 200, 77}
const red_c = gg.Color{230, 41, 55, 255}
const maroon = gg.Color{190, 33, 55, 255}

// 7-segment bit masks (bit 0 = segment A ... bit 6 = segment G)
const seg_digits = [u8(0x3F), 0x06, 0x5B, 0x4F, 0x66, 0x6D, 0x7D, 0x07, 0x7F, 0x6F]!

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	hour   int
	minute int
	second int
	analog bool
}

fn draw_bar(ctx &gg.Context, cx f32, cy f32, len f32, thick f32, vertical bool, c gg.Color) {
	if vertical {
		ctx.draw_polygon_filled(cx, cy - len / 2 - thick / 2, thick * 0.7, 3, 90, c)
		ctx.draw_rect_filled(cx - thick / 2, cy - len / 2, thick, len, c)
		ctx.draw_polygon_filled(cx, cy + len / 2 + thick / 2, thick * 0.7, 3, -90, c)
	} else {
		ctx.draw_polygon_filled(cx - len / 2 - thick / 2, cy, thick * 0.7, 3, 180, c)
		ctx.draw_rect_filled(cx - len / 2, cy - thick / 2, len, thick, c)
		ctx.draw_polygon_filled(cx + len / 2 + thick / 2, cy, thick * 0.7, 3, 0, c)
	}
}

fn draw_digit(ctx &gg.Context, x f32, y f32, value int, on gg.Color, off gg.Color) {
	if value < 0 || value > 9 { return }
	bits := seg_digits[value]
	sl, st := f32(60), f32(20)
	adj := st * 0.3
	draw_bar(ctx, x + st + sl / 2, y + st, sl, st, false, if bits & 1 != 0 { on } else { off })
	draw_bar(ctx, x + st + sl + st / 2, y + 2 * st + sl / 2 - adj, sl, st, true, if bits & 2 != 0 {
		on
	} else {
		off
	})
	draw_bar(ctx, x + st + sl + st / 2, y + 4 * st + sl + sl / 2 - 3 * adj, sl, st, true, if bits & 4 != 0 {
		on
	} else {
		off
	})
	draw_bar(ctx, x + st + sl / 2, y + 5 * st + 2 * sl - 4 * adj, sl, st, false, if bits & 8 != 0 {
		on
	} else {
		off
	})
	draw_bar(ctx, x + st / 2, y + 4 * st + sl + sl / 2 - 3 * adj, sl, st, true, if bits & 16 != 0 {
		on
	} else {
		off
	})
	draw_bar(ctx, x + st / 2, y + 2 * st + sl / 2 - adj, sl, st, true, if bits & 32 != 0 {
		on
	} else {
		off
	})
	draw_bar(ctx, x + st + sl / 2, y + 3 * st + sl - 2 * adj, sl, st, false, if bits & 64 != 0 {
		on
	} else {
		off
	})
}

fn draw_hand(ctx &gg.Context, cx f32, cy f32, length f32, thick f32, angle f32, c gg.Color) {
	rad := f32(math.radians(angle))
	ux, uy := math.cosf(rad), math.sinf(rad)
	px, py := -uy * thick / 2, ux * thick / 2
	ctx.draw_convex_poly([
		cx + px,
		cy + py,
		cx + ux * length + px,
		cy + uy * length + py,
		cx + ux * length - px,
		cy + uy * length - py,
		cx - px,
		cy - py,
	], c)
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	if app.analog {
		cx, cy := f32(400), f32(240)
		ctx.draw_circle_filled(cx, cy, 180, lightgray_t)
		ctx.draw_circle_filled(cx, cy, 12, gray_c)
		for i in 0 .. 60 {
			a := f32(math.radians(f32(i) * 6 - 90))
			inner := f32(160) + if i % 5 == 0 { f32(-4) } else { f32(0) }
			ctx.draw_line(cx + math.cosf(a) * inner, cy + math.sinf(a) * inner,
				cx + math.cosf(a) * 170, cy + math.sinf(a) * 170, darkgray)
		}
		hangle := f32(app.hour % 12) * 30 + f32(app.minute) * 0.5 - 90
		mangle := f32(app.minute) * 6 + f32(app.second) * 0.1 - 90
		sangle := f32(app.second) * 6 - 90
		draw_hand(ctx, cx, cy, 100, 7, hangle, black_c)
		draw_hand(ctx, cx, cy, 130, 7, mangle, darkgray)
		draw_hand(ctx, cx, cy, 140, 3, sangle, maroon)
	} else {
		on, off := red_c, lightgray_t
		draw_digit(ctx, 30, 60, app.hour / 10, on, off)
		draw_digit(ctx, 150, 60, app.hour % 10, on, off)
		colon := if app.second % 2 == 1 { red_c } else { off }
		ctx.draw_circle_filled(270, 130, 12, colon)
		ctx.draw_circle_filled(270, 210, 12, colon)
		draw_digit(ctx, 290, 60, app.minute / 10, on, off)
		draw_digit(ctx, 410, 60, app.minute % 10, on, off)
		ctx.draw_circle_filled(530, 130, 12, colon)
		ctx.draw_circle_filled(530, 210, 12, colon)
		draw_digit(ctx, 550, 60, app.second / 10, on, off)
		draw_digit(ctx, 670, 60, app.second % 10, on, off)
		tm := '${app.hour:02}:${app.minute:02}:${app.second:02}'
		ctx.draw_text(ctx.width / 2 - ctx.text_width(tm) / 2, 300, tm, size: 150, color: black_c)
	}
	mode := if app.analog { 'ANALOGUE CLOCK' } else { 'DIGITAL CLOCK' }
	ctx.draw_text(10, 10, 'Press [SPACE] to switch clock mode: ${mode}',
		size:  20
		color: darkgray
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.analog = !app.analog
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	now := time.now()
	app.hour = now.hour
	app.minute = now.minute
	app.second = now.second
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - digital clock'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
