module main

// Advanced helper starter: pooled particles, immediate-mode UI and charts.
// Run with: v run examples/gg/advanced_base.v
import gg
import time

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	emitter   gg.ParticleEmitter
	theme     gg.UITheme
	input     gg.InputState
	last      time.Time
	values    []f32
	sample_at f32
}

fn main() {
	mut app := &App{
		emitter: gg.new_emitter(gg.ParticleConfig{
			rate:           60
			life:           1.2
			life_variance:  0.4
			speed:          70
			speed_variance: 40
			direction:      -1.5707963
			spread:         1.2
			size:           3
			gravity:        gg.Vec2{0, 120}
			start_color:    gg.rgb(255, 210, 120)
			end_color:      gg.rgba(255, 80, 60, 0)
		})
		theme:   gg.default_ui_theme()
		values:  [f32(2), 5, 3, 8, 6, 9, 4]
	}
	app.emitter.seed(99)
	app.emitter.position = gg.Vec2{300, 230}
	app.last = time.now()

	app.ctx = gg.new_context(
		width:        600
		height:       380
		window_title: 'gg advanced base'
		bg_color:     gg.rgb(18, 20, 28)
		frame_fn:     frame
		event_fn:     on_event
		user_data:    app
	)
	app.ctx.run()
}

fn on_event(e &gg.Event, mut app App) {
	if e.typ == .key_down {
		app.input.on_key_down(e.key_code)
	}
	if e.typ == .key_up {
		app.input.on_key_up(e.key_code)
	}
	if e.typ == .mouse_down {
		app.input.on_mouse_button(e.mouse_button, true, e.mouse_x, e.mouse_y)
	}
	if e.typ == .mouse_up {
		app.input.on_mouse_button(e.mouse_button, false, e.mouse_x, e.mouse_y)
	}
}

fn frame(mut app App) {
	now := time.now()
	mut dt := f32((now - app.last).seconds())
	app.last = now
	if dt > 0.1 {
		dt = 0.1
	}

	app.emitter.update(dt)
	app.sample_at += dt
	if app.sample_at > 0.4 {
		app.sample_at = 0
		app.values << f32(2 + (app.values.len % 7))
		if app.values.len > 24 {
			app.values = app.values[1..]
		}
	}

	app.ctx.begin()
	app.emitter.draw(app.ctx)

	panel := gg.Rect{20, 20, 220, 150}
	gg.ui_panel(app.ctx, panel, app.theme)
	gg.draw_line_chart(app.ctx, app.values, gg.Rect{32, 32, 196, 90}, app.theme.accent)
	gg.draw_bar_chart(app.ctx, app.values, gg.Rect{32, 130, 196, 28}, gg.palette('ocean'))
	clicked := app.input.mouse_just_pressed(.left)
	if gg.ui_button(app.ctx, gg.Rect{20, 185, 120, 30}, 'Burst', app.input.mouse_pos(), clicked,
		app.theme) {
		app.emitter.burst(30, gg.Vec2{300, 230})
	}

	lines := ['advanced helpers', 'particles: ${app.emitter.active_count()}', 'click Burst to emit']
	gg.ui_hud(app.ctx, lines, gg.Rect{360, 20, 210, 72}, app.theme)
	app.ctx.end()

	app.input.end_frame()
}
