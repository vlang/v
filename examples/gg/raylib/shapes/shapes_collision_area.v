module main

// raylib [shapes] example - collision area, ported to gg.
import gg

const raywhite = gg.Color{245, 245, 245, 255}
const black_c = gg.Color{0, 0, 0, 255}
const red_c = gg.Color{230, 41, 55, 255}
const gold = gg.Color{255, 203, 0, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const lime = gg.Color{1, 222, 38, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const upper_limit = 40

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	ax        f32         = 10
	ay        f32
	aw        f32 = 200
	ah        f32 = 100
	aspeed    f32 = 4
	bx        f32
	by        f32
	bw        f32 = 60
	bh        f32 = 60
	cx        f32
	cy        f32
	cw        f32
	ch        f32
	paused    bool
	collision bool
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	bar := if app.collision { red_c } else { black_c }
	ctx.draw_rect_filled(0, 0, f32(ctx.width), upper_limit, bar)

	ctx.draw_rect_filled(app.ax, app.ay, app.aw, app.ah, gold)
	ctx.draw_rect_filled(app.bx, app.by, app.bw, app.bh, blue_c)
	if app.collision {
		ctx.draw_rect_filled(app.cx, app.cy, app.cw, app.ch, lime)
		msg := 'COLLISION!'
		ctx.draw_text(ctx.width / 2 - ctx.text_width(msg) / 2, upper_limit / 2 - 10, msg,
			size:  20
			color: black_c
		)
		area := int(app.cw) * int(app.ch)
		ctx.draw_text(ctx.width / 2 - 100, upper_limit + 10, 'Collision Area: ${area}',
			size:  20
			color: black_c
		)
	}
	ctx.draw_text(20, ctx.height - 35, 'Press SPACE to PAUSE/RESUME',
		size:  20
		color: lightgray
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space {
		app.paused = !app.paused
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if !app.paused {
		app.ax += app.aspeed
	}
	if app.ax + app.aw >= f32(app.ctx.width) || app.ax <= 0 {
		app.aspeed *= -1
	}
	mx, my := f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)
	app.bx = mx - app.bw / 2
	app.by = my - app.bh / 2
	if app.bx + app.bw >= f32(app.ctx.width) {
		app.bx = f32(app.ctx.width) - app.bw
	} else if app.bx <= 0 {
		app.bx = 0
	}
	if app.by + app.bh >= f32(app.ctx.height) {
		app.by = f32(app.ctx.height) - app.bh
	} else if app.by <= upper_limit {
		app.by = upper_limit
	}
	app.collision = app.ax < app.bx + app.bw && app.ax + app.aw > app.bx
		&& app.ay < app.by + app.bh && app.ay + app.ah > app.by
	if app.collision {
		left := if app.ax > app.bx { app.ax } else { app.bx }
		right := if app.ax + app.aw < app.bx + app.bw { app.ax + app.aw } else { app.bx + app.bw }
		top := if app.ay > app.by { app.ay } else { app.by }
		bottom := if app.ay + app.ah < app.by + app.bh { app.ay + app.ah } else { app.by + app.bh }
		app.cx, app.cy = left, top
		app.cw, app.ch = right - left, bottom - top
	}
}

fn main() {
	mut app := &App{}
	app.ay = f32(450) / 2 - 50
	app.bx = f32(800) / 2 - 30
	app.by = f32(450) / 2 - 30
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - collision area'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
