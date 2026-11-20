module main

// Port of raylib textures_portal_window.c
// PORT NOTE: gg has no off-axis/oblique frustum or raw rlgl matrix API. The
// second dimension is rendered with a regular camera shifted by the portal
// offset, and projected onto a thin textured cube in the archway. The
// perspective illusion is approximate rather than mathematically matched.
import gg
import math
import sokol.gfx

const portal_raywhite = gg.Color{245, 245, 245, 255}
const portal_gold = gg.Color{255, 203, 40, 255}
const portal_maroon = gg.Color{128, 0, 0, 255}
const portal_purple = gg.Color{138, 43, 226, 255}
const portal_lime = gg.Color{0, 255, 0, 255}
const portal_gray = gg.Color{128, 128, 128, 255}

fn rt_image(rt &gg.RenderTexture2D, smp gfx.Sampler) gg.Image {
	return gg.Image{
		width:       rt.width
		height:      rt.height
		nr_channels: 4
		ok:          true
		simg_ok:     true
		simg:        rt.texture()
		ssmp:        smp
	}
}

fn flush(ctx &gg.Context) {
	ctx.r.draw()
	ctx.r.rewind()
}

struct App {
mut:
	ctx     &gg.Context         = unsafe { nil }
	rt      &gg.RenderTexture2D = unsafe { nil }
	rt_idx  int                 = -1
	sampler gfx.Sampler
	cam     gg.Camera3D
	other   gg.Camera3D
	time    f32
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0, 2.5, 7}
		target:   gg.Vec3{0, 1.8, 0}
		fovy:     45
	}
	app.other = gg.Camera3D{
		position: gg.Vec3{0, 2.5, -53}
		target:   gg.Vec3{0, 1.8, -60}
		fovy:     45
	}
	app.sampler = gfx.make_sampler(&gfx.SamplerDesc{
		min_filter: .linear
		mag_filter: .linear
		wrap_u:     .clamp_to_edge
		wrap_v:     .clamp_to_edge
	})
	app.rt = gg.new_render_texture2d(480, 640) or { &gg.RenderTexture2D(unsafe { nil }) }
	if !isnil(app.rt) {
		app.rt_idx = app.ctx.cache_image(rt_image(app.rt, app.sampler))
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.time += dt
	app.cam.update(.free, app.ctx, dt)
	app.other.position = app.cam.position + gg.Vec3{0, 0, -60}
	app.other.target = app.cam.target + gg.Vec3{0, 0, -60}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()

	// Dimension B -> render texture.
	if !isnil(app.rt) {
		app.rt.begin(gg.Color{10, 5, 20, 255})
		app.ctx.begin_3d(cam: app.other) or {
			app.rt.end()
			app.ctx.end()
			return
		}
		app.ctx.draw_grid(30, 0.8, gg.Color{80, 80, 80, 255})
		core := gg.Vec3{0, 2 + f32(math.sin(f64(app.time * 2.5))) * 0.4, -64}
		app.ctx.draw_sphere(core, 1.2, 16, 16, portal_purple)
		app.ctx.draw_sphere_wires(core, 1.25, 16, 16, gg.magenta)
		for i in 0 .. 4 {
			angle := app.time * 1.5 + f32(i) * f32(math.pi) / 2
			pos := gg.Vec3{f32(math.sin(f64(angle))) * 2.5, 2 + f32(math.cos(f64(app.time * 3 + f32(i)))) * 0.5, -64 + f32(math.cos(f64(angle))) * 2.5}
			app.ctx.draw_cube(pos, gg.Vec3{0.5, 0.5, 0.5}, portal_lime)
			app.ctx.draw_cube_wires(pos, gg.Vec3{0.52, 0.52, 0.52}, gg.Color{0, 100, 0, 255})
		}
		app.ctx.end_3d()
		flush(app.ctx)
		app.rt.end()
	}

	// Dimension A on screen.
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_grid(20, 1.0, gg.gray)
	app.ctx.draw_cube(gg.Vec3{-3.5, 2, 0}, gg.Vec3{0.8, 4, 0.8}, gg.Color{80, 80, 80, 255})
	app.ctx.draw_cube_wires(gg.Vec3{-3.5, 2, 0}, gg.Vec3{0.8, 4, 0.8}, gg.Color{255, 160, 0, 255})
	app.ctx.draw_cube(gg.Vec3{3.5, 2, 0}, gg.Vec3{0.8, 4, 0.8}, gg.Color{80, 80, 80, 255})
	app.ctx.draw_cube_wires(gg.Vec3{3.5, 2, 0}, gg.Vec3{0.8, 4, 0.8}, gg.Color{255, 160, 0, 255})
	app.ctx.draw_cube_wires(gg.Vec3{0, 2, 0}, gg.Vec3{3.2, 4.2, 0.2}, portal_gold)
	app.ctx.draw_cube(gg.Vec3{0, 0.05, 0}, gg.Vec3{3.4, 0.1, 0.6}, portal_maroon)
	app.ctx.draw_cube(gg.Vec3{0, 2, -0.05}, gg.Vec3{3.1, 4.1, 0.05}, portal_maroon)
	if !isnil(app.rt) {
		portal_img := rt_image(app.rt, app.sampler)
		app.ctx.draw_cube_tex(gg.Vec3{0, 2, 0}, gg.Vec3{3, 4, 0.05}, &portal_img, gg.white)
	}
	app.ctx.end_3d()
	app.ctx.draw_rect_filled(15, 15, 520, 100, gg.Color{0, 0, 0, 191})
	app.ctx.draw_rect_empty(15, 15, 520, 100, portal_gold)
	app.ctx.draw_text(28, 25, 'PORTAL WINDOW', size: 20, color: portal_gold)
	app.ctx.draw_text(28, 52, 'Look through the golden arch into Dimension B',
		size:  20
		color: portal_raywhite
	)
	app.ctx.draw_text(28, 80, 'Controls: Mouse to look | WASD to move',
		size:  20
		color: portal_gray
	)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - portal window'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		bg_color:     portal_raywhite
		depth:        true
	)
	app.ctx.run()
}
