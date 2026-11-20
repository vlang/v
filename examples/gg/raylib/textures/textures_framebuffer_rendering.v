module main

// Port of raylib textures_framebuffer_rendering.c
// PORT NOTE: gg has no matrix-stack / oblique-frustum API. The observer camera
// prism is drawn as a simple wire box at the subject camera position, and each
// view is rendered to its own RenderTexture2D with an ordinary perspective
// camera. Render-texture passes are flushed explicitly before they end.
import gg
import sokol.gfx

const fb_black = gg.Color{0, 0, 0, 255}
const fb_raywhite = gg.Color{245, 245, 245, 255}
const fb_gold = gg.Color{255, 203, 40, 255}
const fb_pink = gg.Color{255, 109, 194, 255}
const fb_darkgray = gg.Color{80, 80, 80, 255}

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
	rt_obs  &gg.RenderTexture2D = unsafe { nil }
	rt_sub  &gg.RenderTexture2D = unsafe { nil }
	obs_idx int                 = -1
	sub_idx int                 = -1
	sampler gfx.Sampler
	obs_cam gg.Camera3D
	sub_cam gg.Camera3D
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.sampler = gfx.make_sampler(&gfx.SamplerDesc{
		min_filter: .linear
		mag_filter: .linear
		wrap_u:     .clamp_to_edge
		wrap_v:     .clamp_to_edge
	})
	app.rt_obs = gg.new_render_texture2d(400, 450) or { &gg.RenderTexture2D(unsafe { nil }) }
	app.rt_sub = gg.new_render_texture2d(400, 450) or { &gg.RenderTexture2D(unsafe { nil }) }
	if !isnil(app.rt_obs) {
		app.obs_idx = app.ctx.cache_image(rt_image(app.rt_obs, app.sampler))
	}
	if !isnil(app.rt_sub) {
		app.sub_idx = app.ctx.cache_image(rt_image(app.rt_sub, app.sampler))
	}
	app.obs_cam = gg.Camera3D{
		position: gg.Vec3{10, 10, 10}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.sub_cam = gg.Camera3D{
		position: gg.Vec3{5, 5, 5}
		target:   gg.Vec3{}
		fovy:     45
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.obs_cam.update(.free, app.ctx, dt)
	app.sub_cam.update(.orbital, app.ctx, dt)
}

fn cleanup(data voidptr) {
	app := unsafe { &App(data) }
	if !isnil(app.rt_obs) {
		mut rt := app.rt_obs
		rt.free()
	}
	if !isnil(app.rt_sub) {
		mut rt := app.rt_sub
		rt.free()
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()

	// Observer view -> left render texture.
	if !isnil(app.rt_obs) {
		app.rt_obs.begin(fb_raywhite)
		app.ctx.begin_3d(cam: app.obs_cam) or {
			app.rt_obs.end()
			app.ctx.end()
			return
		}
		app.ctx.draw_grid(10, 1.0, gg.gray)
		app.ctx.draw_cube(gg.Vec3{}, gg.Vec3{2, 2, 2}, fb_gold)
		app.ctx.draw_cube_wires(gg.Vec3{}, gg.Vec3{2, 2, 2}, fb_pink)
		app.ctx.draw_cube_wires(app.sub_cam.position, gg.Vec3{0.6, 0.6, 0.6}, gg.green)
		app.ctx.draw_line_3d(app.sub_cam.position, gg.Vec3{}, gg.green)
		app.ctx.end_3d()
		app.ctx.draw_text(10, 420, 'Observer View', size: 20, color: fb_black)
		app.ctx.draw_text(10, 10, 'WASD + Mouse to Move', size: 20, color: fb_darkgray)
		app.ctx.draw_text(10, 30, 'Scroll to Zoom', size: 20, color: fb_darkgray)
		flush(app.ctx)
		app.rt_obs.end()
	}

	// Subject view -> right render texture.
	if !isnil(app.rt_sub) {
		app.rt_sub.begin(fb_raywhite)
		app.ctx.begin_3d(cam: app.sub_cam) or {
			app.rt_sub.end()
			app.ctx.end()
			return
		}
		app.ctx.draw_cube(gg.Vec3{}, gg.Vec3{2, 2, 2}, fb_gold)
		app.ctx.draw_cube_wires(gg.Vec3{}, gg.Vec3{2, 2, 2}, fb_pink)
		app.ctx.draw_grid(10, 1.0, gg.gray)
		app.ctx.end_3d()
		app.ctx.draw_text(10, 420, 'Subject View', size: 20, color: fb_black)
		flush(app.ctx)
		app.rt_sub.end()
	}

	// Composite both views on screen.
	if app.obs_idx >= 0 {
		app.ctx.draw_image_with_config(
			img_id:   app.obs_idx
			img_rect: gg.Rect{0, 0, 400, 450}
			flip_y:   true
		)
	}
	if app.sub_idx >= 0 {
		app.ctx.draw_image_with_config(
			img_id:   app.sub_idx
			img_rect: gg.Rect{400, 0, 400, 450}
			flip_y:   true
		)
	}
	app.ctx.draw_line(400, 0, 400, 450, fb_black)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [textures] example - framebuffer rendering'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		cleanup_fn:   cleanup
		bg_color:     fb_black
		depth:        true
	)
	app.ctx.run()
}
