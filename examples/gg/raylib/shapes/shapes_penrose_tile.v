module main

// raylib [shapes] example - penrose tile, ported to gg.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const line_col = gg.Color{0, 0, 0, 51}
const rule_w = 'YF++ZF4-XF[-YF4-WF]++'
const rule_x = '+YF--ZF[3-WF--XF]+'
const rule_y = '-WF++XF[+++YF++ZF]-'
const rule_z = '--YF++++WF[+ZF++++XF]--XF'

struct Turtle {
mut:
	x     f32
	y     f32
	angle f32
}

struct App {
mut:
	ctx         &gg.Context = unsafe { nil }
	production  string      = '[X]++[X]++[X]++[X]++[X]'
	draw_len    f32
	steps       int
	generations int = 1
}

fn build(mut app App) {
	mut out := ''
	for ch in app.production {
		match ch {
			`W` { out += rule_w }
			`X` { out += rule_x }
			`Y` { out += rule_y }
			`Z` { out += rule_z }
			`F` {}
			else { out += '${ch}' }
		}
	}
	app.production = out
	app.draw_len *= 0.5
	app.steps = 0
}

fn (mut app App) rebuild() {
	app.production = '[X]++[X]++[X]++[X]++[X]'
	app.draw_len = 460 * f32(app.generations) / 4
	for _ in 0 .. app.generations {
		build(mut app)
	}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	cx, cy := f32(ctx.width) / 2, f32(ctx.height) / 2
	mut turtle := Turtle{ angle: -90 }
	mut stack := []Turtle{}
	mut repeats := 1
	app.steps += 12
	if app.steps > app.production.len {
		app.steps = app.production.len
	}
	for i in 0 .. app.steps {
		ch := app.production[i]
		if ch == `F` {
			for _ in 0 .. repeats {
				rad := f32(math.radians(turtle.angle))
				nx := turtle.x + app.draw_len * math.cosf(rad)
				ny := turtle.y + app.draw_len * math.sinf(rad)
				ctx.draw_line(cx + turtle.x, cy + turtle.y, cx + nx, cy + ny, line_col)
				turtle.x, turtle.y = nx, ny
			}
			repeats = 1
		} else if ch == `+` {
			turtle.angle += 36 * repeats
			repeats = 1
		} else if ch == `-` {
			turtle.angle -= 36 * repeats
			repeats = 1
		} else if ch == `[` {
			stack << Turtle{turtle.x, turtle.y, turtle.angle}
		} else if ch == `]` {
			if stack.len > 0 {
				turtle = stack.pop()
			}
		} else if ch >= `0` && ch <= `9` {
			repeats = int(ch - `0`)
		}
	}
	ctx.draw_text(10, 10, 'penrose l-system', size: 20, color: darkgray)
	ctx.draw_text(10, 30, 'press up or down to change generations', size: 20, color: darkgray)
	ctx.draw_text(10, 50, 'generations: ${app.generations}', size: 20, color: darkgray)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down {
		return
	}
	old := app.generations
	match e.key_code {
		.up {
			if app.generations < 4 { app.generations++ }
		}
		.down {
			if app.generations > 0 { app.generations-- }
		}
		else {}
	}
	if old != app.generations && app.generations > 0 {
		app.rebuild()
	}
}

fn main() {
	mut app := &App{}
	app.rebuild()
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - penrose tile'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		bg_color:     raywhite
	)
	app.ctx.run()
}
