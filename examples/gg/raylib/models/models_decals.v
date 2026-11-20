module main

// Port of raylib models_decals.c
// PORT NOTE: the clip-space decal projection (GenMeshDecal with six clipping
// planes) has no gg equivalent. Decals are approximated as small textured
// cubes placed at the hit point, and raygui buttons are plain text/HUD rects.
import gg
import sokol.gfx

const dc_raywhite = gg.Color{245, 245, 245, 255}
const dc_lime = gg.Color{0, 255, 0, 255}
const dc_yellow = gg.Color{255, 255, 0, 255}

struct Decal {
mut:
	pos  gg.Vec3
	size f32
}

struct App {
mut:
	ctx       &gg.Context = unsafe { nil }
	cam       gg.Camera3D
	model     gg.Model3D
	has_model bool
	tex       gg.Image
	decals    []Decal
	show      bool = true
	max_decals int = 256
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{5, 5, 5}
		target:   gg.Vec3{0, 1, 0}
		fovy:     45
	}
	app.tex = app.ctx.create_image('resources/models/obj/character_diffuse.png') or { gg.Image{} }
	app.model = gg.load_model('resources/models/obj/character.obj') or { gg.Model3D{} }
	app.has_model = app.model.valid()
	if app.has_model {
		app.model.material.texture = &app.tex
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.mouse_buttons.has(.right) {
		app.cam.update(.free, app.ctx, 1.0 / 60.0)
	}
	ray := gg.screen_to_world_ray(f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y), app.cam, 800, 450)
	mut hit := gg.RayHit{}
	if app.has_model {
		hit = gg.ray_mesh(ray, app.model.mesh, app.model.pos, app.model.rotation, 1.0)
	}
	if hit.hit && app.ctx.mouse_buttons.has(.left) && app.decals.len < app.max_decals {
		app.decals << Decal{pos: hit.point, size: 0.35}
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
	if app.show && app.has_model {
		app.ctx.draw_model(&app.model, app.cam)
	}
	for d in app.decals {
		app.ctx.draw_cube(d.pos, gg.Vec3{d.size, d.size, d.size}, dc_yellow)
	}
	if hit.hit {
		app.ctx.draw_cube(hit.point, gg.Vec3{0.4, 0.4, 0.4}, gg.Color{255, 255, 255, 128})
	}
	app.ctx.draw_grid(10, 10.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(500, 10, 'Vertices / Triangles', size: 10, color: dc_lime)
	app.ctx.draw_text(500, 25, 'Decals: ${app.decals.len} (clipping unavailable)', size: 10, color: dc_lime)
	app.ctx.draw_text(10, 430, 'Hold RMB to move camera | Left click: place decal',
		size: 10, color: gg.gray)
	app.ctx.draw_text(10, 380, 'Hide Model', size: 14, color: dc_lime)
	app.ctx.draw_text(120, 380, 'Clear Decals', size: 14, color: dc_lime)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - decals'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     dc_raywhite
		depth:        true
	)
	app.ctx.run()
}
