module main

// raylib [core] example - input multitouch
// gg reports touch points through .touches_* events; we keep the last known
// position of up to ten fingers and draw a numbered circle for each.

import gg

const screen_width = 800
const screen_height = 450
const max_touch_points = 10

const ray_white = gg.Color{245, 245, 245, 255}
const dark_gray = gg.Color{80, 80, 80, 255}
const orange = gg.Color{255, 161, 0, 255}
const black = gg.Color{0, 0, 0, 255}

struct App {
mut:
	gg        &gg.Context = unsafe { nil }
	touches   [max_touch_points]gg.Vec2
	touch_cnt int
}

fn main() {
	mut app := &App{}
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - input multitouch'
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
		.touches_began, .touches_moved {
			n := if e.num_touches < max_touch_points { e.num_touches } else { max_touch_points }
			app.touch_cnt = n
			for i in 0 .. n {
				if i < e.touches.len {
					app.touches[i] = gg.Vec2{e.touches[i].pos_x, e.touches[i].pos_y}
				}
			}
		}
		.touches_ended, .touches_cancelled {
			app.touch_cnt = 0
		}
		else {}
	}
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	for i in 0 .. app.touch_cnt {
		p := app.touches[i]
		if p.x > 0 && p.y > 0 {
			ctx.draw_circle_filled(p.x, p.y, 34, orange)
			ctx.draw_text(int(p.x) - 10, int(p.y) - 70, i.str(), size: 40, color: black)
		}
	}
	ctx.draw_text(10, 10, 'touch the screen at multiple locations to get multiple balls',
		size:  20
		color: dark_gray
	)
	ctx.end()
}
