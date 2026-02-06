@[has_globals]
module main

import gg
import rand

__global app = App{}

struct Ball {
mut:
	x     f32
	y     f32
	dx    f32
	dy    f32
	color gg.Color
}

struct App {
mut:
	size  gg.Size = gg.Size{1024, 768}
	balls []Ball
}

fn main() {
	init_app()
	gg.start(
		window_title: 'Bouncing balls'
		width:        app.size.width
		height:       app.size.height
		frame_fn:     on_frame
	)
}

fn on_frame(ctx &gg.Context) {
	app.size = gg.window_size()
	if ctx.frame % 20000 == 0 {
		init_app()
	}
	for mut ball in app.balls {
		move_ball(mut ball)
	}
	ctx.begin()
	for ball in app.balls {
		ctx.draw_circle_filled(ball.x, app.size.height - ball.y, 6, ball.color)
	}
	ctx.draw_text(10, 20, 'Frame: ${ctx.frame:06}', color: gg.Color{255, 255, 255, 255}, size: 20)
	ctx.end()
}

fn init_app() {
	app.balls.clear()
	for _ in 0 .. 2500 {
		app.balls << Ball{
			x:     rand.f32() * app.size.width
			y:     3 * app.size.height / 4
			dx:    5 * rand.f32()
			dy:    5 * rand.f32()
			color: gg.Color{rand.u8(), rand.u8(), rand.u8(), 255}
		}
	}
}

@[inline]
fn move_ball(mut ball Ball) {
	ball.x += ball.dx
	ball.y += ball.dy
	ball.dy -= 9.81 * 0.01
	if ball.x <= 0 || ball.x >= app.size.width - 1 {
		ball.dx = -ball.dx
		apply_friction(mut ball)
	}
	if ball.y <= 0 || ball.y >= app.size.height - 1 {
		ball.dy = -ball.dy
		apply_friction(mut ball)
	}
	// apply some clipping:
	ball.x = f32_max(-5, f32_min(f32(app.size.width) * 1.3, ball.x))
	ball.y = f32_max(-5, f32_min(f32(app.size.height) * 1.3, ball.y))
	ball.dx = f32_max(-50, f32_min(30, ball.dx))
	ball.dy = f32_max(-50, f32_min(30, ball.dy))
}

@[inline]
fn apply_friction(mut ball Ball) {
	// apply some random friction on each bounce:
	ball.dy *= (1 - rand.f32() * 0.1)
	ball.dx *= (1 - rand.f32() * 0.01)
}
