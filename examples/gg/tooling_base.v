module main

// Tooling starter: procedural maze, geometry helpers, debug overlay and save.
// Run with: v run examples/gg/tooling_base.v
import gg
import gg.game
import gg.fx
import gg.debug
import time

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	maze   []bool
	gizmos debug.Gizmos
	graph  debug.FrameGraph
	log    debug.DebugLog
	save   debug.SaveStore
	input  game.InputState
	theme  fx.UITheme
	last   time.Time
	hull   []gg.Vec2
}

fn main() {
	mut app := &App{
		maze:  game.generate_maze(21, 13, 1234)
		theme: fx.default_ui_theme()
		save:  debug.new_save_store()
	}
	app.log.add('tooling base ready')
	app.save.set_int('visits', app.save.get_int('visits', 0) + 1)
	app.last = time.now()
	app.hull = game.convex_hull([
		gg.Vec2{300, 60},
		gg.Vec2{360, 100},
		gg.Vec2{340, 160},
		gg.Vec2{280, 170},
		gg.Vec2{260, 110},
		gg.Vec2{310, 110},
	])

	app.ctx = gg.new_context(
		width:        420
		height:       300
		window_title: 'gg tooling base'
		bg_color:     gg.rgb(16, 18, 24)
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.ctx.run()
}

fn on_event(e &gg.Event, mut app App) {
	if e.typ == .key_down {
		app.input.on_key_down(e.key_code)
		if e.key_code == .s {
			app.save.set_int('visits', app.save.get_int('visits', 0))
			app.log.add('save pressed')
		}
	}
	if e.typ == .key_up {
		app.input.on_key_up(e.key_code)
	}
	if e.typ == .mouse_move {
		app.input.on_mouse_move(e.mouse_x, e.mouse_y)
	}
}

fn frame(mut app App) {
	now := time.now()
	mut dt := f32((now - app.last).seconds())
	app.last = now
	if dt > 0.1 {
		dt = 0.1
	}
	app.graph.push(dt)

	app.gizmos.clear()
	mouse := app.input.mouse_pos()
	app.gizmos.cross(mouse, 12, gg.yellow)
	app.gizmos.rect(gg.Vec2{10, 10}, gg.Vec2{130, 70}, gg.cyan)

	app.ctx.begin()
	for row in 0 .. 13 {
		for col in 0 .. 21 {
			if app.maze[row * 21 + col] {
				app.ctx.draw_rect_filled(f32(col) * 12 + 140, f32(row) * 12 + 10, 11, 11,
					gg.rgb(50, 70, 90))
			}
		}
	}
	for i in 0 .. app.hull.len {
		a := app.hull[i]
		b := app.hull[(i + 1) % app.hull.len]
		app.ctx.draw_line(a.x, a.y, b.x, b.y, gg.rgb(220, 120, 180))
	}
	app.gizmos.draw(app.ctx)
	app.graph.draw(app.ctx, gg.Rect{10, 80, 120, 30}, gg.rgb(90, 200, 140))
	app.log.draw(app.ctx, gg.Rect{10, 120, 120, 80}, app.theme)
	app.ctx.end()

	app.input.end_frame()
}
