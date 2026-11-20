module main

// Port of raylib models_mesh_generation.c
// PORT NOTE: gg has no hemisphere or knot generators. The hemisphere is
// approximated with a low-ring sphere and the knot with a torus; all other
// shapes use gg's gen_mesh_* family (including torus, cone and polygon).
import gg

const mg_raywhite = gg.Color{245, 245, 245, 255}
const mg_skyblue = gg.Color{135, 206, 235, 255}
const mg_darkblue = gg.Color{0, 42, 141, 255}
const mg_darkgray = gg.Color{80, 80, 80, 255}

const num_models = 9

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	cam    gg.Camera3D
	meshes [num_models]gg.Mesh3D
	models [num_models]gg.Model3D
	check  gg.Image
	current int
}

fn cache_img(ctx &gg.Context, img gg.Image) int {
	mut copy := img
	copy.nr_channels = 4
	copy.init_sokol_image()
	return ctx.cache_image(copy)
}

fn custom_triangle() gg.Mesh3D {
	return gg.Mesh3D{
		vertices: [gg.Vec3{0, 0, 0}, gg.Vec3{1, 0, 2}, gg.Vec3{2, 0, 0}]!
		normals:  [gg.Vec3{0, 1, 0}, gg.Vec3{0, 1, 0}, gg.Vec3{0, 1, 0}]!
		uvs:      [f32(0), 0, 0.5, 1, 1, 0]!
		indices:  [u32(0), 1, 2]!
	}
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{5, 5, 5}
		target:   gg.Vec3{}
		fovy:     45
	}
	// A 2x2 checked texture built directly in a pixel buffer.
	mut pixels := []u8{len: 2 * 2 * 4}
	red := gg.Color{230, 41, 55, 255}
	green := gg.Color{0, 228, 48, 255}
	for i in 0 .. 4 {
		c := if i % 2 == 0 { red } else { green }
		unsafe {
			pixels[i * 4] = c.r
			pixels[i * 4 + 1] = c.g
			pixels[i * 4 + 2] = c.b
			pixels[i * 4 + 3] = 255
		}
	}
	app.check = gg.Image{width: 2, height: 2, nr_channels: 4, ok: true, data: pixels.data}
	_ = cache_img(app.ctx, app.check)

	app.meshes[0] = gg.gen_mesh_plane(2, 2, 4)
	app.meshes[1] = gg.gen_mesh_cube(gg.Vec3{2, 1, 2})
	app.meshes[2] = gg.gen_mesh_sphere(2, 16, 16)
	app.meshes[3] = gg.gen_mesh_sphere(2, 4, 16)
	app.meshes[4] = gg.gen_mesh_cylinder(gg.Vec3{0, -1.5, 0}, gg.Vec3{0, 0.5, 0}, 1.5, 16)
	app.meshes[5] = gg.gen_mesh_torus(gg.Vec3{}, 4.0, 0.25, 16, 32)
	app.meshes[6] = gg.gen_mesh_torus(gg.Vec3{}, 2.0, 1.0, 16, 32)
	app.meshes[7] = gg.gen_mesh_polygon(gg.Vec3{}, 2, 5, 0.5)
	app.meshes[8] = custom_triangle()
	for i in 0 .. num_models {
		app.models[i] = gg.Model3D{mesh: &app.meshes[i]}
		app.models[i].material.texture = &app.check
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_model(&app.models[app.current], app.cam)
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_rect_filled(30, 400, 310, 30, gg.Color{135, 206, 235, 128})
	app.ctx.draw_rect_empty(30, 400, 310, 30, mg_darkblue)
	app.ctx.draw_text(40, 410, 'MOUSE LEFT BUTTON to CYCLE PROCEDURAL MODELS', size: 10, color: mg_skyblue)
	names := ['PLANE', 'CUBE', 'SPHERE', 'HEMISPHERE', 'CYLINDER', 'TORUS', 'KNOT', 'POLY', 'Custom (triangle)']
	app.ctx.draw_text(560, 10, names[app.current], size: 20, color: mg_darkgray)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if (e.typ == .mouse_button_down && e.mouse_button == .left) || e.key_code == .right {
		app.current = (app.current + 1) % num_models
	} else if e.key_code == .left {
		app.current = (app.current + num_models - 1) % num_models
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - mesh generation'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     mg_raywhite
		depth:        true
	)
	app.ctx.run()
}
