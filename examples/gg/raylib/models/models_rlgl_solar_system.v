module main

// Port of raylib models_rlgl_solar_system.c
// PORT NOTE: gg has no rlgl matrix stack. Orbit positions are computed with
// trigonometry each frame and spheres are drawn at those world positions.
import gg
import math

const ss_raywhite = gg.Color{245, 245, 245, 255}
const ss_gold = gg.Color{255, 203, 40, 255}
const ss_blue = gg.Color{0, 121, 241, 255}
const ss_maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	ctx              &gg.Context = unsafe { nil }
	cam              gg.Camera3D
	earth_rot        f32
	earth_orbit_rot  f32
	moon_rot         f32
	moon_orbit_rot   f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{16, 16, 16}
		target:   gg.Vec3{}
		fovy:     45
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.earth_rot += 5 * 0.2
	app.earth_orbit_rot += (365.0 / 360.0) * (5 * 0.2) * 0.2
	app.moon_rot += 2 * 0.2
	app.moon_orbit_rot += 8 * 0.2

	eo := app.earth_orbit_rot * f32(math.pi) / 180.0
	earth := gg.Vec3{8 * f32(math.cos(f64(eo))), 0, 8 * f32(math.sin(f64(eo)))}
	mo := app.moon_orbit_rot * f32(math.pi) / 180.0
	moon := earth + gg.Vec3{1.5 * f32(math.cos(f64(mo))), 0, 1.5 * f32(math.sin(f64(mo)))}

	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_sphere(gg.Vec3{}, 4, 16, 16, ss_gold)
	app.ctx.draw_sphere(earth, 0.6, 16, 16, ss_blue)
	app.ctx.draw_sphere(moon, 0.16, 16, 16, gg.Color{211, 211, 211, 255})

	// Earth orbit reference ring.
	mut i := 0
	for i < 64 {
		a0 := f32(i) / 64.0 * 2 * f32(math.pi)
		a1 := f32(i + 1) / 64.0 * 2 * f32(math.pi)
		app.ctx.draw_line_3d(gg.Vec3{8 * f32(math.cos(f64(a0))), 0, 8 * f32(math.sin(f64(a0)))},
			gg.Vec3{8 * f32(math.cos(f64(a1))), 0, 8 * f32(math.sin(f64(a1)))},
			gg.Color{255, 0, 0, 128})
		i++
	}
	app.ctx.draw_grid(20, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(400, 10, 'EARTH ORBITING AROUND THE SUN!', size: 20, color: ss_maroon)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - rlgl solar system'
		user_data:    app
		frame_fn:     frame
		bg_color:     ss_raywhite
		depth:        true
	)
	app.ctx.run()
}
