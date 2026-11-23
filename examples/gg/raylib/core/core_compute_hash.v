module main

// raylib [core] example - compute hash
// Computes CRC32, MD5, SHA1 and SHA256 of a text string and base64-encodes
// it. PORT NOTE: raygui's text box is replaced with a minimal editable
// field driven by .char/.key_down events.

import gg
import crypto.md5
import crypto.sha1
import crypto.sha256
import encoding.base64
import encoding.hex

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const black = gg.Color{0, 0, 0, 255}
const gray = gg.Color{130, 130, 130, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const blue = gg.Color{0, 121, 241, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	input    string      = 'The quick brown fox jumps over the lazy dog.'
	editing  bool
	computed bool
	crc32    string
	md5      string
	sha1     string
	sha256   string
	base64   string
}

fn crc32(data []u8) u32 {
	mut crc := u32(0xffffffff)
	for b in data {
		crc ^= u32(b)
		for _ in 0 .. 8 {
			mask := u32(0) - (crc & 1)
			crc = (crc >> 1) ^ (0xedb88320 & mask)
		}
	}
	return crc ^ 0xffffffff
}

fn compute(mut app App) {
	data := app.input.bytes()
	app.crc32 = '${crc32(data):08x}'
	app.md5 = hex.encode(md5.sum(data))
	app.sha1 = hex.encode(sha1.sum(data))
	app.sha256 = hex.encode(sha256.sum(data))
	app.base64 = base64.encode_str(app.input)
	app.computed = true
}

fn main() {
	mut app := &App{}
	compute(mut app)
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - compute hash'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	match e.typ {
		.mouse_down {
			app.editing = e.mouse_y >= 64 && e.mouse_y <= 96
		}
		.key_down {
			if !e.key_repeat && e.key_code == .backspace && app.input.len > 0 {
				app.input = app.input[0..app.input.len - 1]
				compute(mut app)
			}
			if e.key_code == .enter {
				compute(mut app)
			}
		}
		.char {
			if app.editing && e.char_code >= 32 && e.char_code < 127 {
				app.input += rune(e.char_code).str()
				compute(mut app)
			}
		}
		else {}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.draw_text(40, 26, 'INPUT DATA (TEXT):', size: 20, color: black)
	ctx.draw_rect(x: 40, y: 64, w: 720, h: 32, color: if app.editing { light_gray } else { gg.white })
	ctx.draw_rect_empty(40, 64, 720, 32, gray)
	ctx.draw_text(46, 70, app.input + (if app.editing { '_' } else { '' }),
		size:  10
		color: black
	)
	ctx.draw_rect(x: 40, y: 104, w: 720, h: 32, color: blue)
	ctx.draw_text(200, 110, 'COMPUTE INPUT DATA HASHES', size: 10, color: gg.white)
	ctx.draw_text(40, 160, 'INPUT DATA HASH VALUES:', size: 20, color: black)
	rows := [
		'CRC32 [32 bit]:',
		'MD5 [128 bit]:',
		'SHA1 [160 bit]:',
		'SHA256 [256 bit]:',
	]
	values := [app.crc32, app.md5, app.sha1, app.sha256]
	for i, row in rows {
		y := 200 + 36 * i
		ctx.draw_text(40, y, row, size: 10, color: black)
		ctx.draw_rect(x: 160, y: f32(y), w: 600, h: 28, color: light_gray)
		ctx.draw_text(166, y + 4, values[i], size: 10, color: black)
	}
	ctx.draw_text(40, 380, 'BASE64 ENCODING:', size: 20, color: maroon)
	ctx.draw_rect(x: 160, y: 380, w: 600, h: 28, color: light_gray)
	ctx.draw_text(166, 384, app.base64, size: 10, color: black)
	_ = app.computed
	ctx.end()
}
