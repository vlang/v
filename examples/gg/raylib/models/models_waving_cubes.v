module main

// Port of raylib models_waving_cubes.c
// PORT NOTE: gg has no ColorFromHSV(); a small local HSV conversion is used.
import gg
import math

const wc_raywhite = gg.Color{245, 245, 245, 255}

fn hsv(h int, s f32, v f32) gg.Color {
	hf := f32(h % 360) / 60.0
	c := v * s
	x := c * (1 - math.abs(f32(math.fmod(f64(hf), 2.0)) - 1))
	m := v - c
	mut r := f32(0)
	mut g := f32(0)
	mut b := f32(0)
	match int(hf) {
		0 {
			r = c
			g = x
		}
		1 {
			r = x
			g = c
		}
		2 {
			g = c
			b = x
		}
		3 {
			g = x
			b = c
		}
		4 {
			r = x
			b = c
		}
		else {
			r = c
			b = x
		}
	}
	return gg.Color{u8((r + m) * 255), u8((g + m) * 255), u8((b + m) * 255), 255}
}

struct App {
mut:
	ctx  &gg.Context = unsafe { nil }
	cam  gg.Camera3D
	time f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{30, 20, 30}
		target:   gg.Vec3{}
		fovy:     70
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.time += 1.0 / 60.0
	scale := (2.0 + f32(math.sin(f64(app.time)))) * 0.7
	ct := app.time * 0.3
	app.cam.position = gg.Vec3{40 * f32(math.cos(f64(ct))), 20, 40 * f32(math.sin(f64(ct)))}

	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_grid(10, 5.0, gg.gray)
	for x in 0 .. 15 {
		for y in 0 .. 15 {
			for z in 0 .. 15 {
				block_scale := f32(x + y + z) / 30.0
				scatter := f32(math.sin(f64(block_scale * 20.0 + app.time * 4.0)))
				pos := gg.Vec3{(f32(x) - 7.5) * (scale * 3.0) + scatter, (f32(y) - 7.5) * (scale * 2.0) + scatter, (f32(z) - 7.5) * (scale * 3.0) + scatter}
				c := hsv((x + y + z) * 18, 0.75, 0.9)
				size := (2.4 - scale) * block_scale
				if size > 0 {
					app.ctx.draw_cube(pos, gg.Vec3{size, size, size}, c)
				}
			}
		}
	}
	app.ctx.end_3d()
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - waving cubes'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     wc_raywhite
		depth:        true
	)
	app.ctx.run()
}
