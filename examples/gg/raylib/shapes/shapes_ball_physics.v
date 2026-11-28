module main

// raylib [shapes] example - ball physics, ported to gg.
import gg
import math
import rand

const max_balls = 5000
const raywhite = gg.Color{245, 245, 245, 255}
const darkgray = gg.Color{80, 80, 80, 255}
const black_c = gg.Color{0, 0, 0, 255}
const blue_c = gg.Color{0, 121, 241, 255}

struct Ball {
mut:
	x          f32
	y          f32
	vx         f32
	vy         f32
	prev_x     f32
	prev_y     f32
	radius     f32
	friction   f32 = 0.99
	elasticity f32 = 0.9
	color      gg.Color
	grabbed    bool
}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	balls      []Ball
	ball_count int = 1
	grabbed    int = -1
	press_x    f32
	press_y    f32
	gravity    f32 = 100
}

fn rand_range(lo int, hi int) int {
	return lo + (rand.intn(hi - lo + 1) or { 0 })
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	for i in 0 .. app.ball_count {
		b := app.balls[i]
		ctx.draw_circle_filled(b.x, b.y, b.radius, b.color)
		ctx.draw_circle_empty(b.x, b.y, b.radius, black_c)
	}
	text_y := ctx.height
	ctx.draw_text(10, 10, 'grab a ball by pressing with the mouse and throw it by releasing',
		size:  10
		color: darkgray
	)
	ctx.draw_text(10, 30,
		'right click to create new balls (keep left control pressed to create a lot)',
		size:  10
		color: darkgray
	)
	ctx.draw_text(10, 50, 'use mouse wheel to change gravity',
		size:  10
		color: darkgray
	)
	ctx.draw_text(10, 70, 'middle click to shake',
		size:  10
		color: darkgray
	)
	ctx.draw_text(10, text_y - 70, 'BALL COUNT: ${app.ball_count}',
		size:  20
		color: black_c
	)
	ctx.draw_text(10, text_y - 40, 'GRAVITY: ${app.gravity:.2f}',
		size:  20
		color: black_c
	)
	ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	mx, my := f32(e.mouse_x), f32(e.mouse_y)
	match e.typ {
		.mouse_down {
			if e.mouse_button == .left {
				for i := app.ball_count - 1; i >= 0; i-- {
					app.press_x = mx - app.balls[i].x
					app.press_y = my - app.balls[i].y
					dx := mx - app.balls[i].x
					dy := my - app.balls[i].y
					if math.sqrtf(dx * dx + dy * dy) <= app.balls[i].radius {
						app.balls[i].grabbed = true
						app.grabbed = i
						break
					}
				}
			} else if e.mouse_button == .right && app.ball_count < max_balls {
				app.balls << Ball{
					x:      mx
					y:      my
					vx:     f32(rand_range(-300, 300))
					vy:     f32(rand_range(-300, 300))
					radius: 20 + f32(rand_range(0, 30))
					color:  gg.Color{u8(rand_range(0, 255)), u8(rand_range(0, 255)), u8(rand_range(0, 255)), 255}
				}
				app.ball_count++
			} else if e.mouse_button == .middle {
				for i in 0 .. app.ball_count {
					if !app.balls[i].grabbed {
						app.balls[i].vx = f32(rand_range(-2000, 2000))
						app.balls[i].vy = f32(rand_range(-2000, 2000))
					}
				}
			}
		}
		.mouse_up {
			if e.mouse_button == .left && app.grabbed >= 0 {
				app.balls[app.grabbed].grabbed = false
				app.grabbed = -1
			}
		}
		.mouse_scroll {
			app.gravity += e.scroll_y * 5
		}
		else {}
	}
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	// PORT NOTE: gg does not expose window screen position, so the raylib "shake on
	// window move" feature is omitted. Middle-click shake is kept.
	mouse_x := f32(app.ctx.mouse_pos_x)
	mouse_y := f32(app.ctx.mouse_pos_y)
	for i in 0 .. app.ball_count {
		mut b := app.balls[i]
		if !b.grabbed {
			b.x += b.vx * dt
			b.y += b.vy * dt
			w := f32(app.ctx.width)
			h := f32(app.ctx.height)
			if b.x + b.radius >= w {
				b.x = w - b.radius
				b.vx = -b.vx * b.elasticity
			} else if b.x - b.radius <= 0 {
				b.x = b.radius
				b.vx = -b.vx * b.elasticity
			}
			if b.y + b.radius >= h {
				b.y = h - b.radius
				b.vy = -b.vy * b.elasticity
			} else if b.y - b.radius <= 0 {
				b.y = b.radius
				b.vy = -b.vy * b.elasticity
			}
			b.vx *= b.friction
			b.vy = b.vy * b.friction + app.gravity
		} else {
			b.x = mouse_x - app.press_x
			b.y = mouse_y - app.press_y
			if dt > 0 {
				b.vx = (b.x - b.prev_x) / dt
				b.vy = (b.y - b.prev_y) / dt
			}
			b.prev_x = b.x
			b.prev_y = b.y
		}
		app.balls[i] = b
	}
}

fn main() {
	mut app := &App{
		balls: [Ball{
			x:      400
			y:      225
			vx:     200
			vy:     200
			radius: 40
			color:  blue_c
		}]
	}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - ball physics'
		user_data:    app
		frame_fn:     on_frame
		event_fn:     on_event
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
