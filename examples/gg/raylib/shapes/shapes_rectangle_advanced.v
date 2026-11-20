module main

// raylib [shapes] example - rectangle advanced, ported to gg.
// PORT NOTE: gg has no per-vertex gradients. Each rounded bar is drawn as vertical strips
// whose colors interpolate horizontally, then rounded corners are overlaid.
import gg
import math

const raywhite = gg.Color{245, 245, 245, 255}
const blue_c = gg.Color{0, 121, 241, 255}
const red_c = gg.Color{230, 41, 55, 255}
const pink = gg.Color{255, 189, 196, 255}
const black_c = gg.Color{0, 0, 0, 255}

fn mix(a gg.Color, b gg.Color, t f32) gg.Color {
	return gg.Color{
		r: u8(f32(a.r) * (1 - t) + f32(b.r) * t)
		g: u8(f32(a.g) * (1 - t) + f32(b.g) * t)
		b: u8(f32(a.b) * (1 - t) + f32(b.b) * t)
		a: 255
	}
}

fn draw_round_grad(ctx &gg.Context, x f32, y f32, w f32, h f32, rl f32, rr f32, left gg.Color, right gg.Color) {
	for i in 0 .. 100 {
		c := mix(left, right, f32(i) / 99)
		ctx.draw_rect_filled(x + w * f32(i) / 100, y, w / 100 + 1, h, c)
	}
	rl_r := h * rl / 2
	rr_r := h * rr / 2
	ctx.draw_rect_filled(x, y - rl_r / 2, rl_r, h + rl_r, left)
	ctx.draw_rect_filled(x + w - rr_r, y - rr_r / 2, rr_r, h + rr_r, right)
	if rl > 0 {
		ctx.draw_circle_filled(x + rl_r, y, rl_r, left)
		ctx.draw_circle_filled(x + rl_r, y + h, rl_r, left)
	}
	if rr > 0 {
		ctx.draw_circle_filled(x + w - rr_r, y, rr_r, right)
		ctx.draw_circle_filled(x + w - rr_r, y + h, rr_r, right)
	}
	_ = math.absf(rl)
}

struct App {
mut:
	ctx &gg.Context = unsafe { nil }
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	w := f32(ctx.width) / 2
	h := f32(ctx.height) / 6
	x := f32(ctx.width) / 2 - w / 2
	y := f32(ctx.height) / 2 - 5 * (h / 2)
	draw_round_grad(ctx, x, y, w, h, 0.8, 0.8, blue_c, red_c)
	y += h + 1
	draw_round_grad(ctx, x, y, w, h, 0.5, 1.0, red_c, pink)
	y += h + 1
	draw_round_grad(ctx, x, y, w, h, 1.0, 0.5, red_c, blue_c)
	y += h + 1
	draw_round_grad(ctx, x, y, w, h, 0.0, 1.0, blue_c, black_c)
	y += h + 1
	draw_round_grad(ctx, x, y, w, h, 1.0, 0.0, blue_c, pink)
	ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [shapes] example - rectangle advanced'
		user_data:    app
		frame_fn:     on_frame
		bg_color:     raywhite
	)
	app.ctx.run()
}
