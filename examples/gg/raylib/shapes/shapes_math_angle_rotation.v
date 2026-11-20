module main

// raylib [shapes] example - math angle rotation, ported to gg.
import gg
import math

const (
	white_c   = gg.Color{255, 255, 255, 255}
	lightgray = gg.Color{200, 200, 200, 255}
	green_c   = gg.Color{0, 158, 47, 255}
	orange_c  = gg.Color{255, 161, 0, 255}
	blue_c    = gg.Color{0, 121, 241, 255}
	magenta_c = gg.Color{255, 0, 255, 255}
)

fn hsv(h f32) gg.Color {
	c := f32(0.9)
	x := c * (1 - math.absf((h / 60) % 2 - 1))
	m := f32(0)
	r, g, b := f32(0), f32(0), f32(0)
	if h < 60 { r, g = c, x } else if h < 120 { r, g = x, c } else if h < 180 { g, b = c, x } else if h < 240 { g, b = x, c } else if h < 300 { r, b = x, c } else { r, b = c, x }
	return gg.Color{u8((r + m) * 255), u8((g + m) * 255), u8((b + m) * 255), 255}
}

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	angle f32
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	cx, cy := f32(ctx.width) / 2, f32(ctx.height) / 2
	const len = f32(150)
	ctx.draw_text(10, 10, 'Fixed angles + rotating line', size: 20, color: lightgray)
	angles := [4]int{0, 30, 60, 90}
	cols := [4]gg.Color{green_c, orange_c, blue_c, magenta_c}
	for i in 0 .. 4 {
		rad := f32(math.radians(angles[i]))
		ex := cx + math.cosf(rad) * len
		ey := cy + math.sinf(rad) * len
		ctx.draw_line(cx, cy, ex, ey, cols[i])
		ctx.draw_text(int(cx + math.cosf(rad) * (len + 20)), int(cy + math.sinf(rad) * (len + 20)),
			'${angles[i]}°', size: 20, color: cols[i])
	}
	anim := f32(math.radians(app.angle))
	ctx.draw_line(cx, cy, cx + math.cosf(anim) * len, cy + math.sinf(anim) * len,
		hsv(app.angle % 360))
	ctx.end()
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	app.angle++
	if app.angle >= 360 { app.angle -= 360 }
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        720
		height:       400
		window_title: 'raylib [shapes] example - math angle rotation'
		user_data:    app
		frame_fn:     on_frame
		update_fn:    on_update
		bg_color:     white_c
	)
	app.ctx.run()
}
