module main

// Port of raylib textures_clipboard_image.c
// PORT NOTE: gg has no clipboard image API (GetClipboardImage). Ctrl+V cannot
// paste an image from the system clipboard; the collection UI and R-to-reset
// behaviour are kept, and a bundled image can be added with the number keys.
import gg

const clip_raywhite = gg.Color{245, 245, 245, 255}
const clip_black = gg.Color{0, 0, 0, 255}
const clip_lightgray = gg.Color{211, 211, 211, 255}

const max_collection = 20

struct Item {
mut:
	img gg.Image
	x   f32
	y   f32
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	items   []Item
	demo    gg.Image
	message string
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.demo = app.ctx.create_image('resources/raylib_logo.png') or { gg.Image{} }
	app.message = 'Ctrl+V to Paste (unavailable in gg) - R to Reset - 1 to add demo'
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	for i in 0 .. app.items.len {
		it := app.items[i]
		app.ctx.draw_image(it.x, it.y, f32(it.img.width), f32(it.img.height), &it.img)
	}
	app.ctx.draw_rect_filled(0, 0, 800, 40, clip_black)
	app.ctx.draw_text(20, 10, 'Clipboard Image - ${app.message}', size: 16, color: clip_lightgray)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	if e.key_code == .r {
		app.items.clear()
		app.message = 'collection reset'
	} else if e.key_code == gg.KeyCode._1 {
		if app.items.len < max_collection && app.demo.ok {
			app.items << Item{ img: app.demo, x: f32(app.ctx.mouse_pos_x), y: f32(app.ctx.mouse_pos_y) }
			app.message = 'demo image added (clipboard unavailable)'
		}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - clipboard image'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     clip_raywhite
	)
	app.ctx.run()
}
