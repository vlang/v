module main

// raylib [core] example - random sequence
// A shuffled sequence of coloured bars. SPACE reshuffles, UP/DOWN change
// the number of bars. gg uses the V rand module in place of LoadRandomSequence.

import gg
import rand

const screen_width = 800
const screen_height = 450

const ray_white = gg.Color{245, 245, 245, 255}
const black = gg.Color{0, 0, 0, 255}
const maroon = gg.Color{128, 0, 0, 255}

struct ColorRect {
mut:
	color  gg.Color
	x      f32
	y      f32
	width  f32
	height f32
}

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	rect_count int         = 20
	rects      []ColorRect = []
}

fn main() {
	mut app := &App{}
	app.rects = generate(app.rect_count)
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - random sequence'
		bg_color:     ray_white
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.gg.fps.show = true
	app.gg.fps.x = screen_width - 80
	app.gg.fps.y = 10
	app.gg.run()
}

fn generate(count int) []ColorRect {
	mut out := []ColorRect{}
	bar_width := f32(screen_width) / f32(count)
	start_x := (f32(screen_width) - f32(count) * bar_width) * 0.5
	for i in 0 .. count {
		h := f32(rand.intn(count) or { 0 }) / f32(count - 1) * f32(screen_height) * 0.75
		out << ColorRect{
			color:  gg.Color{u8(rand.intn(256) or { 0 }), u8(rand.intn(256) or { 0 }), u8(rand.intn(256) or { 0 }), 255}
			x:      start_x + f32(i) * bar_width
			y:      f32(screen_height) * 0.75 - h
			width:  bar_width
			height: h
		}
	}
	return out
}

fn shuffle(mut app App) {
	for _ in 0 .. app.rect_count {
		i := rand.intn(app.rect_count) or { 0 }
		j := rand.intn(app.rect_count) or { 0 }
		tmp := app.rects[i]
		app.rects[i].color = app.rects[j].color
		app.rects[i].height = app.rects[j].height
		app.rects[i].y = app.rects[j].y
		app.rects[j].color = tmp.color
		app.rects[j].height = tmp.height
		app.rects[j].y = tmp.y
	}
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down || e.key_repeat {
		return
	}
	match e.key_code {
		.space {
			shuffle(mut app)
		}
		.up {
			app.rect_count++
			app.rects = generate(app.rect_count)
		}
		.down {
			if app.rect_count >= 4 {
				app.rect_count--
				app.rects = generate(app.rect_count)
			}
		}
		else {}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	for r in app.rects {
		ctx.draw_rect(x: r.x, y: r.y, w: r.width, h: r.height, color: r.color)
	}
	ctx.draw_text(10, screen_height - 96, 'Press SPACE to shuffle the current sequence',
		size:  20
		color: black
	)
	ctx.draw_text(10, screen_height - 64, 'Press UP to add a rectangle and generate a new sequence',
		size:  20
		color: black
	)
	ctx.draw_text(10, screen_height - 32,
		'Press DOWN to remove a rectangle and generate a new sequence',
		size:  20
		color: black
	)
	ctx.draw_text(10, 10, 'Count: ${app.rect_count} rectangles', size: 20, color: maroon)
	ctx.end()
}
