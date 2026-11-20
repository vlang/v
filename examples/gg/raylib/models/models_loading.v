module main

// Port of raylib models_loading.c
// PORT NOTE: gg loads OBJ/glTF/GLB via load_model(), but has no drag&drop file
// API. Loading at startup is kept; the drop-to-replace and drop-to-retexture
// interactions are omitted. Picking uses gg's ray_box() against a computed AABB.
import gg
import math

const ld_raywhite = gg.Color{245, 245, 245, 255}
const ld_gray = gg.Color{128, 128, 128, 255}
const ld_green = gg.Color{0, 255, 0, 255}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	model    gg.Model3D
	texture  gg.Image
	has_model bool
	selected bool
	box_c    gg.Vec3
	box_s    gg.Vec3
}

fn bounds(mesh &gg.Mesh3D) (gg.Vec3, gg.Vec3) {
	mut lo := gg.Vec3{1e9, 1e9, 1e9}
	mut hi := gg.Vec3{-1e9, -1e9, -1e9}
	for v in mesh.vertices {
		lo.x = math.min(f64(lo.x), f64(v.x))
		lo.y = math.min(f64(lo.y), f64(v.y))
		lo.z = math.min(f64(lo.z), f64(v.z))
		hi.x = math.max(f64(hi.x), f64(v.x))
		hi.y = math.max(f64(hi.y), f64(v.y))
		hi.z = math.max(f64(hi.z), f64(v.z))
	}
	c := gg.Vec3{(lo.x + hi.x) / 2, (lo.y + hi.y) / 2, (lo.z + hi.z) / 2}
	s := gg.Vec3{hi.x - lo.x, hi.y - lo.y, hi.z - lo.z}
	return c, s
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{50, 50, 50}
		target:   gg.Vec3{0, 12, 0}
		fovy:     45
	}
	app.texture = app.ctx.create_image('resources/models/obj/castle_diffuse.png') or { gg.Image{} }
	app.model = gg.load_model('resources/models/obj/castle.obj') or { gg.Model3D{} }
	if app.model.valid() {
		app.has_model = true
		app.model.material.texture = &app.texture
		app.box_c, app.box_s = bounds(app.model.mesh)
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.mouse_buttons.has(.left) {
		ray := gg.screen_to_world_ray(f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y), app.cam, 800, 450)
		app.selected = gg.ray_box(ray, app.box_c, app.box_s).hit
	}
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	if app.has_model {
		app.ctx.draw_model(&app.model, app.cam)
	}
	app.ctx.draw_grid(20, 10.0, gg.gray)
	if app.selected {
		app.ctx.draw_cube_wires(app.box_c, app.box_s, ld_green)
	}
	app.ctx.end_3d()
	app.ctx.draw_text(10, 430, 'Drag & drop model to load mesh/texture (unavailable in gg)',
		size: 10, color: ld_gray)
	if app.selected {
		app.ctx.draw_text(800 - 110, 10, 'MODEL SELECTED', size: 10, color: ld_green)
	}
	app.ctx.draw_text(600, 430, '(c) Castle 3D model by Alberto Cano', size: 10, color: ld_gray)
	app.ctx.end()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.orbital, app.ctx, dt)
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - loading'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		bg_color:     ld_raywhite
		depth:        true
	)
	app.ctx.run()
}
