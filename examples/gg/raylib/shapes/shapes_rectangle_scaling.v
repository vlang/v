module main

// raylib [shapes] example - rectangle scaling, ported to gg.
import gg

const mark = f32(12)
const raywhite = gg.Color{245, 245, 245, 255}
const gray_c = gg.Color{130, 130, 130, 255}
const red_c = gg.Color{230, 41, 55, 255}
const green_t = gg.Color{0, 158, 47, 128}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	x       f32         = 100
	y       f32         = 100
	w       f32         = 200
	h       f32         = 80
	ready   bool
	scaling bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(10, 10, 'Scale rectangle dragging from bottom-right corner!',
		size:  20
		color: gray_c
	)
	ctx.draw_rect_filled(app.x, app.y, app.w, app.h, green_t)
	if app.ready {
		ctx.draw_rect_empty(app.x, app.y, app.w, app.h, red_c)
		ctx.draw_triangle_filled(app.x + app.w - mark, app.y + app.h,
			app.x + app.w, app.y + app.h, app.x + app.w, app.y + app.h - mark, red_c)
	}
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	mx, my := f32(e.mouse_x), f32(e.mouse_y)
	if e.typ == .mouse_down && e.mouse_button == .left && app.ready {
		app.scaling = true
	}
	if e.typ == .mouse_up && e.mouse_button == .left {
		app.scaling = false
	}
	if e.typ == .mouse_move && app.scaling {
		app.w = mx - app.x
		app.h = my - app.y
		if app.w < mark { app.w = mark }
		if app.h < mark { app.h = mark }
		if app.w > f32(app.ctx.width) - app.x { app.w = f32(app.ctx.width) - app.x }
		if app.h > f32(app.ctx.height) - app.y { app.h = f32(app.ctx.height) - app.y }
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	app.ready = mx >= app.x + app.w - mark && mx <= app.x + app.w
		&& my >= app.y + app.h - mark && my <= app.y + app.h
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - rectangle scaling'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
