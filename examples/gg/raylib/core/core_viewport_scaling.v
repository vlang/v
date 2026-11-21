module main

// raylib [core] example - viewport scaling
// Cycles through six ways of fitting a low-resolution game view into the
// window (integer/non-integer, aspect/height/width preserving). The virtual
// mouse position on the rendered texture is shown as a lime circle.

import gg

const resolution_count = 4
const viewport_type_count = 6

const black = gg.Color{0, 0, 0, 255}
const white = gg.Color{255, 255, 255, 255}
const light_gray = gg.Color{200, 200, 200, 179}
const blue = gg.Color{0, 121, 241, 255}
const sky_blue = gg.Color{102, 191, 255, 255}
const lime = gg.Color{124, 252, 0, 255}

struct App {
mut:
	gg             &gg.Context = unsafe { nil }
	resolution_idx int
	viewport_type  int
	game_width     int = 64
	game_height    int = 64
	source         gg.Rect
	dest           gg.Rect
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [core] example - viewport scaling'
		bg_color:     black
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	resize_render(mut app)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .mouse_down || e.mouse_button != .left {
		return
	}
	mx, my := f32(e.mouse_x), f32(e.mouse_y)
	inside := fn (x f32, y f32, r gg.Rect) bool {
		return x >= r.x && x <= r.x + r.width && y >= r.y && y <= r.y + r.height
	}
	resolutions := [gg.Vec2{64, 64}, gg.Vec2{256, 240}, gg.Vec2{320, 180}, gg.Vec2{3840, 2160}]
	if inside(mx, my, gg.Rect{200, 30, 10, 10}) {
		app.resolution_idx = (app.resolution_idx + resolution_count - 1) % resolution_count
	} else if inside(mx, my, gg.Rect{215, 30, 10, 10}) {
		app.resolution_idx = (app.resolution_idx + 1) % resolution_count
	} else if inside(mx, my, gg.Rect{200, 45, 10, 10}) {
		app.viewport_type = (app.viewport_type + viewport_type_count - 1) % viewport_type_count
	} else if inside(mx, my, gg.Rect{215, 45, 10, 10}) {
		app.viewport_type = (app.viewport_type + 1) % viewport_type_count
	} else {
		return
	}
	app.game_width = int(resolutions[app.resolution_idx].x)
	app.game_height = int(resolutions[app.resolution_idx].y)
	resize_render(mut app)
}

fn resize_render(mut app App) {
	w, h := app.gg.width, app.gg.height
	gw, gh := app.game_width, app.game_height
	match app.viewport_type {
		0 {
			rx, ry := w / gw, h / gh
			ratio := f32(if rx < ry { rx } else { ry })
			app.source = gg.Rect{0, f32(gh), f32(gw), -f32(gh)}
			app.dest = gg.Rect{f32(int((f32(w) - f32(gw) * ratio) * 0.5)), f32(int((f32(h) - f32(gh) * ratio) * 0.5)), f32(int(f32(gw) * ratio)), f32(int(f32(gh) * ratio))}
		}
		1 {
			ratio := f32(h) / f32(gh)
			sw := f32(int(f32(w) / ratio))
			app.source = gg.Rect{0, 0, sw, -f32(gh)}
			app.dest = gg.Rect{f32(int((f32(w) - sw * ratio) * 0.5)), f32(int((f32(h) - f32(gh) * ratio) * 0.5)), f32(int(sw * ratio)), f32(int(f32(gh) * ratio))}
		}
		2 {
			ratio := f32(w) / f32(gw)
			sh := f32(int(f32(h) / ratio))
			app.source = gg.Rect{0, 0, f32(gw), -sh}
			app.dest = gg.Rect{f32(int((f32(w) - f32(gw) * ratio) * 0.5)), f32(int((f32(h) - sh * ratio) * 0.5)), f32(int(f32(gw) * ratio)), f32(int(sh * ratio))}
		}
		3 {
			rx, ry := f32(w) / f32(gw), f32(h) / f32(gh)
			ratio := if rx < ry { rx } else { ry }
			app.source = gg.Rect{0, f32(gh), f32(gw), -f32(gh)}
			app.dest = gg.Rect{f32(int((f32(w) - f32(gw) * ratio) * 0.5)), f32(int((f32(h) - f32(gh) * ratio) * 0.5)), f32(int(f32(gw) * ratio)), f32(int(f32(gh) * ratio))}
		}
		4 {
			ratio := f32(h) / f32(gh)
			sw := f32(int(f32(w) / ratio))
			app.source = gg.Rect{0, 0, sw, -f32(gh)}
			app.dest = gg.Rect{f32(int((f32(w) - sw * ratio) * 0.5)), f32(int((f32(h) - f32(gh) * ratio) * 0.5)), f32(int(sw * ratio)), f32(int(f32(gh) * ratio))}
		}
		5 {
			ratio := f32(w) / f32(gw)
			sh := f32(int(f32(h) / ratio))
			app.source = gg.Rect{0, 0, f32(gw), -sh}
			app.dest = gg.Rect{f32(int((f32(w) - f32(gw) * ratio) * 0.5)), f32(int((f32(h) - sh * ratio) * 0.5)), f32(int(f32(gw) * ratio)), f32(int(sh * ratio))}
		}
		else {}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	mx, my := f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y)
	tex_mouse := gg.Vec2{mx - app.dest.x, my - app.dest.y}

	ctx.begin()
	ctx.set_viewport(int(app.dest.x), int(app.dest.y), int(app.dest.width), int(app.dest.height))
	ctx.draw_rect(x: 0, y: 0, w: app.dest.width, h: app.dest.height, color: white)
	ctx.draw_circle_filled(tex_mouse.x, tex_mouse.y, 20, lime)
	ctx.set_viewport(0, 0, ctx.width, ctx.height)

	ctx.draw_rect(x: 5, y: 5, w: 330, h: 105, color: light_gray)
	ctx.draw_rect_empty(5, 5, 330, 105, blue)
	names := ['KEEP_ASPECT_INTEGER', 'KEEP_HEIGHT_INTEGER', 'KEEP_WIDTH_INTEGER', 'KEEP_ASPECT',
		'KEEP_HEIGHT', 'KEEP_WIDTH']
	ctx.draw_text(15, 15, 'Window Resolution: ${ctx.width} x ${ctx.height}',
		size:  10
		color: black
	)
	ctx.draw_text(15, 30, 'Game Resolution: ${app.game_width} x ${app.game_height}',
		size:  10
		color: black
	)
	ctx.draw_text(15, 45, 'Type: ${names[app.viewport_type]}', size: 10, color: black)
	ctx.draw_text(15, 60,
		'Scale ratio: ${app.dest.width / app.source.width:.2f} x ${app.dest.height / -app.source.height:.2f}',
		size:  10
		color: black
	)
	ctx.draw_text(15, 75, 'Source size: ${app.source.width:.2f} x ${-app.source.height:.2f}',
		size:  10
		color: black
	)
	ctx.draw_text(15, 90, 'Destination size: ${app.dest.width:.2f} x ${app.dest.height:.2f}',
		size:  10
		color: black
	)
	ctx.draw_rect(x: 200, y: 45, w: 10, h: 10, color: sky_blue)
	ctx.draw_rect(x: 215, y: 45, w: 10, h: 10, color: sky_blue)
	ctx.draw_rect(x: 200, y: 30, w: 10, h: 10, color: sky_blue)
	ctx.draw_rect(x: 215, y: 30, w: 10, h: 10, color: sky_blue)
	ctx.draw_text(203, 46, '<', size: 10, color: black)
	ctx.draw_text(218, 46, '>', size: 10, color: black)
	ctx.draw_text(203, 31, '<', size: 10, color: black)
	ctx.draw_text(218, 31, '>', size: 10, color: black)
	ctx.end()
}
