module main

// Port of raylib models_procedural_decals.c
// PORT NOTE: the clip-space decal mesh generator (six plane clips, custom UVs)
// is not available in gg. Small coloured quads/cubes are placed at the ray hit
// point instead, and raygui buttons are drawn as plain HUD rectangles.
import gg

const pd_raywhite = gg.Color{245, 245, 245, 255}
const pd_lime = gg.Color{0, 255, 0, 255}
const pd_yellow = gg.Color{255, 255, 0, 255}

struct Decal {
mut:
	pos gg.Vec3
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	cam     gg.Camera3D
	mesh    gg.Mesh3D
	model   gg.Model3D
	checker gg.Image
	decals  []Decal
	show    bool = true
}

fn cache_img(ctx &gg.Context, img gg.Image) int {
	mut copy := img
	copy.nr_channels = 4
	copy.init_sokol_image()
	return ctx.cache_image(copy)
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0, 2.5, 5}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.mesh = gg.gen_mesh_torus(gg.Vec3{}, 1.8, 0.8, 32, 16)
	app.model = gg.Model3D{mesh: &app.mesh}

	mut pixels := []u8{len: 512 * 512 * 4}
	light := gg.Color{211, 211, 211, 255}
	gray := gg.Color{128, 128, 128, 255}
	for y in 0 .. 512 {
		for x in 0 .. 512 {
			c := if ((x / 32) + (y / 32)) % 2 == 0 { light } else { gray }
			i := (y * 512 + x) * 4
			unsafe {
				pixels[i] = c.r
				pixels[i + 1] = c.g
				pixels[i + 2] = c.b
				pixels[i + 3] = 255
			}
		}
	}
	app.checker = gg.Image{width: 512, height: 512, nr_channels: 4, ok: true, data: pixels.data}
	_ = cache_img(app.ctx, app.checker)
	app.model.material.texture = &app.checker
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.mouse_buttons.has(.right) {
		app.cam.update(.free, app.ctx, 1.0 / 60.0)
	}
	ray := gg.screen_to_world_ray(f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y), app.cam, 800, 450)
	hit := gg.ray_mesh(ray, app.mesh, gg.Vec3{}, gg.Vec3{}, 1.0)
	if hit.hit && app.ctx.mouse_buttons.has(.left) && app.decals.len < 256 {
		app.decals << Decal{pos: hit.point}
	}
	if app.ctx.mouse_buttons.has(.left) && f32(app.ctx.mouse_pos_y) > 370 && f32(app.ctx.mouse_pos_x) < 230 {
		app.decals.clear()
	}
	if f32(app.ctx.mouse_pos_x) < 110 && f32(app.ctx.mouse_pos_y) > 370 {
		app.show = !app.show
	}

	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	if app.show {
		app.ctx.draw_model(&app.model, app.cam)
	}
	for d in app.decals {
		app.ctx.draw_cube(d.pos, gg.Vec3{0.3, 0.3, 0.3}, pd_yellow)
	}
	if hit.hit {
		app.ctx.draw_cube_wires(hit.point, gg.Vec3{0.35, 0.35, 0.35}, gg.Color{255, 255, 255, 128})
	}
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(520, 10, 'Vertices / Triangles', size: 10, color: pd_lime)
	app.ctx.draw_text(520, 25, 'Decals: ${app.decals.len} (clip unavailable)', size: 10, color: pd_lime)
	app.ctx.draw_text(10, 380, 'Hide Model', size: 14, color: pd_lime)
	app.ctx.draw_text(120, 380, 'Clear Decals', size: 14, color: pd_lime)
	app.ctx.draw_text(10, 430, 'Left click: place decal | Right drag: orbit', size: 10, color: gg.gray)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - procedural decals'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     pd_raywhite
		depth:        true
	)
	app.ctx.run()
}
