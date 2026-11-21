module main

// raylib [core] example - directory files
// Browse directories with a simple list. The raygui list view is replaced
// with clickable rows drawn using gg primitives.

import gg
import os

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const black = gg.Color{0, 0, 0, 255}
const gray = gg.Color{130, 130, 130, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const sky_blue = gg.Color{102, 191, 255, 255}
const white = gg.Color{255, 255, 255, 255}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	directory string
	files     []string
	scroll    int
	active    int = -1
}

fn main() {
	mut app := &App{}
	app.directory = os.getwd()
	refresh(mut app)
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - directory files'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn refresh(mut app App) {
	app.files = os.ls(app.directory) or { []string{} }
	app.files.sort()
	app.scroll = 0
	app.active = -1
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_down && e.mouse_button == .left {
		mx, my := f32(e.mouse_x), f32(e.mouse_y)
		if mx >= 40 && mx <= 88 && my >= 10 && my <= 38 {
			app.directory = os.dir(app.directory)
			refresh(mut app)
			return
		}
		if my >= 50 {
			idx := int((my - 50) / 20) + app.scroll
			if idx >= 0 && idx < app.files.len {
				app.active = idx
				path := if app.directory == '.' {
					app.files[idx]
				} else {
					'${app.directory}/${app.files[idx]}'
				}
				if os.is_dir(path) {
					app.directory = path
					refresh(mut app)
				}
			}
		}
	}
	if e.typ == .mouse_scroll {
		app.scroll -= int(e.scroll_y)
		if app.scroll < 0 {
			app.scroll = 0
		}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_rect(x: 40, y: 10, w: 48, h: 28, color: sky_blue)
	ctx.draw_rect_empty(40, 10, 48, 28, gray)
	ctx.draw_text(54, 15, '<', size: 20, color: black)
	ctx.draw_text(100, 15, app.directory, size: 16, color: black)
	for i in 0 .. 19 {
		idx := app.scroll + i
		if idx >= app.files.len {
			break
		}
		y := 50 + i * 20
		name := app.files[idx]
		if idx == app.active {
			ctx.draw_rect(x: 0, y: f32(y), w: screen_width, h: 20, color: sky_blue)
		} else if i % 2 == 0 {
			ctx.draw_rect(x: 0, y: f32(y), w: screen_width, h: 20, color: light_gray)
		}
		label := if os.is_dir(if app.directory == '.' { name } else { '${app.directory}/${name}' }) {
			'[DIR] ${name}'
		} else {
			name
		}
		ctx.draw_text(40, y + 2, label,
			size:  14
			color: if idx == app.active { white } else { black }
		)
	}
	ctx.end()
}
