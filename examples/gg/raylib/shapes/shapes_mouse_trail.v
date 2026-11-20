module main

// raylib [shapes] example - mouse trail, ported to gg.
import gg

const max_trail = 30
const black_c = gg.Color{0, 0, 0, 255}
const lightgray = gg.Color{200, 200, 200, 255}
const white_c = gg.Color{255, 255, 255, 255}
const skyblue = gg.Color{102, 191, 255, 255}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	trail []gg.Vec2
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	for i in 0 .. app.trail.len {
		p := app.trail[i]
		if p.x == 0 && p.y == 0 {
			continue
		}
		ratio := f32(max_trail - i) / max_trail
		c := skyblue
		c.a = u8(255 * (ratio * 0.5 + 0.5))
		ctx.draw_circle_filled(p.x, p.y, 15 * ratio, c)
	}
	mx, my := f32(ctx.mouse_pos_x), f32(ctx.mouse_pos_y)
	ctx.draw_circle_filled(mx, my, 15, white_c)
	ctx.draw_text(10, ctx.height - 30, 'Move the mouse to see the trail effect!',
		size:  20
		color: lightgray
	)
	ctx.end()
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	if app.trail.len == 0 {
		app.trail = []gg.Vec2{len: max_trail}
	}
	for i := max_trail - 1; i > 0; i-- {
		app.trail[i] = app.trail[i - 1]
	}
	app.trail[0] = gg.Vec2{f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y)}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - mouse trail'
		user_data:    app
		frame_fn:     on_frame
		update_fn:    on_update
		bg_color:     black_c
	)
	app.ctx.run()
}
