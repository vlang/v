module main

// raylib [shapes] example - lines drawing, ported to gg.
// PORT NOTE: gg has no offscreen canvas in the legacy 2D API. The drawing surface is
// approximated with a translucent fade background; strokes accumulate across frames.
import gg
import math

const canvas_bg = gg.Color{245, 245, 245, 16}
const raywhite = gg.Color{245, 245, 245, 255}
const lightgray = gg.Color{200, 200, 200, 255}

fn hsv(h f32, s f32, v f32) gg.Color {
	c := v * s
	x := c * (1 - math.absf((h / 60) % 2 - 1))
	m := v - c
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
	ctx        &gg.Context = unsafe { nil }
	start_text bool        = true
	prev_x     f32
	prev_y     f32
	hue        f32
	thick      f32 = 8
	seg_x1     f32
	seg_y1     f32
	seg_x2     f32
	seg_y2     f32
	seg_color  gg.Color
	has_seg    bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	if app.has_seg {
		ctx.draw_circle_filled(app.seg_x1, app.seg_y1, app.thick / 2, app.seg_color)
		ctx.draw_circle_filled(app.seg_x2, app.seg_y2, app.thick / 2, app.seg_color)
		ctx.draw_line(app.seg_x1, app.seg_y1, app.seg_x2, app.seg_y2, app.seg_color)
	}
	mx, my := f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y)
	left_down := ctx.mouse_buttons.has(.left)
	if !left_down {
		ctx.draw_circle_empty(mx, my, app.thick / 2, gg.Color{127, 127, 127, 127})
	}
	if app.start_text {
		ctx.draw_text(275, 215, 'try clicking and dragging!', size: 20, color: lightgray)
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_down {
		if e.mouse_button == .left {
			app.start_text = false
		} else if e.mouse_button == .middle {
			// No canvas to clear; the fade background handles it over time.
		}
	}
	if e.typ == .mouse_scroll {
		app.thick = math.max(f32(1), math.min(f32(500), app.thick + e.scroll_y))
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	left_down := app.ctx.mouse_buttons.has(.left)
	right_down := app.ctx.mouse_buttons.has(.right)
	if left_down {
		dist := math.sqrtf((mx - app.prev_x) * (mx - app.prev_x) + (my - app.prev_y) * (my - app.prev_y))
		app.hue += dist / 3
		for app.hue >= 360 {
			app.hue -= 360
		}
	}
	if left_down || right_down {
		app.seg_color = if left_down { hsv(app.hue, 1, 1) } else { raywhite }
		app.seg_x1, app.seg_y1 = app.prev_x, app.prev_y
		app.seg_x2, app.seg_y2 = mx, my
		app.has_seg = true
	} else {
		app.has_seg = false
	}
	app.prev_x, app.prev_y = mx, my
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - lines drawing'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     canvas_bg
	)
	app.ctx.run()
}
