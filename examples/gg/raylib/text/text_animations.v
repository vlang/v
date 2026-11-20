module main

// raylib [text] example - animations, ported to gg.
import gg
import math
import rand

const (
	msg = 'This is an animated text!'
	raywhite = gg.Color{245, 245, 245, 255}
	gray_c   = gg.Color{130, 130, 130, 255}
	purple_t = gg.Color{200, 122, 255, 255}
	names = ['Waving', 'Flashing', 'Shaking']!
)

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	delta    f32 = 24
	duration f32 = 60
	offset   f32 = 6
	anim     int
	frames   int
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	ctx.draw_text(20, 20, 'Duration (A | D): ${int(app.duration)}', size: 20, color: gray_c)
	ctx.draw_text(20, 50, 'Delta (W | S): ${app.delta:.1f}', size: 20, color: gray_c)
	ctx.draw_text(20, 80, 'Offset (Z | X): ${app.offset:.1f}', size: 20, color: gray_c)
	ctx.draw_text(20, 110, 'Current Animation (Q | E): ${names[app.anim]}', size: 20, color: gray_c)

	mut x := f32(200)
	for i, ch in msg {
		chstr := '${ch}'
		cw := f32(ctx.text_width(chstr)) + 4
		t := f32((app.frames + int(f32(i) * app.offset)) % int(app.duration))
		gx := x
		gy := f32(250)
		alpha := u8(255)
		match app.anim {
			0 {
				if t < app.duration / 2 {
					gy = 250 + f32(ease_sine_in(f64(t) / f64(app.duration))) * app.delta
				} else {
					gy = 250 + app.delta - f32(ease_sine_out(f64(t) / f64(app.duration))) * app.delta
				}
			}
			1 {
				alpha = u8(255 * (1 - ease_sine_in_out(f64(t) / f64(app.duration))))
			}
			2 {
				dx := f32(rand.intn(int(app.delta)) or { 0 } - int(app.delta) / 2)
				dy := f32(rand.intn(int(app.delta)) or { 0 } - int(app.delta) / 2)
				gx += dx
				gy += dy
			}
			else {}
		}
		col := purple_t
		col.a = alpha
		ctx.draw_text(int(gx), int(gy), chstr, size: 30, color: col)
		x += cw
	}
	ctx.end()
}

fn ease_sine_in(t f64) f64 { return 1 - math.cos(t * math.pi / 2) }
fn ease_sine_out(t f64) f64 { return math.sin(t * math.pi / 2) }
fn ease_sine_in_out(t f64) f64 { return -(math.cos(math.pi * t) - 1) / 2 }

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ != .key_down { return }
	match e.key_code {
		.s { if app.delta > 10 { app.delta -= 2 } }
		.w { if app.delta < 60 { app.delta += 2 } }
		.a { if app.duration > 20 { app.duration -= 5 } }
		.d { if app.duration < 120 { app.duration += 5 } }
		.z { if app.offset > 0 { app.offset-- } }
		.x { if app.offset < 15 { app.offset++ } }
		.q { app.anim = if app.anim == 0 { 2 } else { app.anim - 1 } }
		.e { app.anim = (app.anim + 1) % 3 }
		else {}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	app.frames++
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - animations'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
