module main

// A procedural/CGI starter: seeded fbm noise, a palette-colored height field
// and a Catmull-Rom curve. Run with: v run examples/gg/procedural_base.v
import gg
import gg.anim

struct App {
mut:
	ctx   &gg.Context = unsafe { nil }
	noise anim.Noise
}

fn main() {
	mut app := &App{
		noise: anim.new_noise(1337)
	}
	app.ctx = gg.new_context(
		width:        560
		height:       340
		window_title: 'gg procedural base'
		bg_color:     gg.rgb(18, 20, 28)
		frame_fn:     frame
		user_data:    app
	)
	app.ctx.run()
}

fn frame(mut app App) {
	app.ctx.begin()

	// A height field driven by fractal noise and colored by a palette.
	mut x := f32(20)
	for i in 0 .. 58 {
		n := app.noise.fbm2d(f32(i) * 0.11, 3.2, 4, 2.0, 0.5)
		h := (n + 1) * 55
		c := anim.palette_gradient(anim.palette('forest'), (n + 1) / 2)
		app.ctx.draw_rect_filled(x, 190 - h, 8, h, c)
		x += 9
	}

	// A smooth spline sampled through four control points.
	pts := [
		gg.Vec2{40, 300},
		gg.Vec2{170, 235},
		gg.Vec2{360, 315},
		gg.Vec2{520, 250},
	]
	mut prev := anim.sample_catmull(pts, 0)
	for k in 1 .. 80 {
		t := f32(k) / 79
		p := anim.sample_catmull(pts, t)
		app.ctx.draw_line(prev.x, prev.y, p.x, p.y, gg.cyan)
		prev = p
	}

	// Value noise used as a soft animated-looking texture strip.
	for i in 0 .. 110 {
		v := app.noise.value2d(f32(i) * 0.07, 8.5)
		shade := u8(128 + v * 90)
		app.ctx.draw_rect_filled(20 + f32(i) * 4.7, 325, 4, 8, gg.rgb(shade, shade, shade))
	}

	app.ctx.end()
}
