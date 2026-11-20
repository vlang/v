module main

// raylib [core] example - storage values
// Saves and loads two integers in a small binary file. V's os.read_bytes /
// os.write_bytes replace raylib's SaveFileData / LoadFileData.

import gg
import os
import rand

const screen_width = 800
const screen_height = 450
const storage_file = 'storage.data'

const ray_white = gg.Color{245, 245, 245, 255}
const maroon = gg.Color{128, 0, 0, 255}
const black = gg.Color{0, 0, 0, 255}
const light_gray = gg.Color{200, 200, 200, 255}
const lime = gg.Color{124, 252, 0, 255}

struct App {
mut:
	gg           &gg.Context = unsafe { nil }
	score        int
	hiscore      int
	frames_count int
}

fn save_value(position int, value int) bool {
	data := os.read_bytes(storage_file) or { []u8{} }
	mut buf := data.clone()
	needed := (position + 1) * 4
	if buf.len < needed {
		buf.resize(needed, 0)
	}
	for i in 0 .. 4 {
		buf[position * 4 + i] = u8((u32(value) >> u32(i * 8)) & 0xff)
	}
	os.write_bytes(storage_file, buf) or { return false }
	return true
}

fn load_value(position int) int {
	data := os.read_bytes(storage_file) or { return 0 }
	if data.len < (position + 1) * 4 {
		return 0
	}
	mut v := u32(0)
	for i in 0 .. 4 {
		v |= u32(data[position * 4 + i]) << u32(i * 8)
	}
	return int(v)
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - storage values'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down || e.key_repeat {
		return
	}
	match e.key_code {
		.r {
			app.score = rand.int_in_range(1000, 2000) or { 1000 }
			app.hiscore = rand.int_in_range(2000, 4000) or { 2000 }
		}
		.enter {
			save_value(0, app.score)
			save_value(1, app.hiscore)
		}
		.space {
			app.score = load_value(0)
			app.hiscore = load_value(1)
		}
		else {}
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg
	app.frames_count++
	ctx.begin()
	ctx.draw_text(280, 130, 'SCORE: ${app.score}', size: 40, color: maroon)
	ctx.draw_text(210, 200, 'HI-SCORE: ${app.hiscore}', size: 50, color: black)
	ctx.draw_text(10, 10, 'frames: ${app.frames_count}', size: 20, color: lime)
	ctx.draw_text(220, 40, 'Press R to generate random numbers', size: 20, color: light_gray)
	ctx.draw_text(250, 310, 'Press ENTER to SAVE values', size: 20, color: light_gray)
	ctx.draw_text(252, 350, 'Press SPACE to LOAD values', size: 20, color: light_gray)
	ctx.end()
}
