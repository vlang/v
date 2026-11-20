module main

// Port of raylib models_cubicmap_rendering.c
// PORT NOTE: gg's gen_mesh_cubicmap() creates one flat layer of cubes at y=0
// for non-zero cells; raylib stacks cubes by cell colour and supports a real
// 3D maze layout. The source image is still shown beside the generated model.
import gg

const cm_raywhite = gg.Color{245, 245, 245, 255}
const cm_gray = gg.Color{128, 128, 128, 255}
const cm_green = gg.Color{0, 255, 0, 255}

fn img_pixel(img &gg.Image, x int, y int) gg.Color {
	if img.data == unsafe { nil } || x < 0 || y < 0 || x >= img.width || y >= img.height {
		return gg.Color{}
	}
	d := unsafe { &u8(img.data) }
	i := (y * img.width + x) * 4
	return unsafe { gg.Color{d[i], d[i + 1], d[i + 2], d[i + 3]} }
}

struct App {
mut:
	ctx        &gg.Context = unsafe { nil }
	cam        gg.Camera3D
	map_img    gg.Image
	atlas      gg.Image
	mesh       gg.Mesh3D
	model      gg.Model3D
	map_pos    gg.Vec3 = gg.Vec3{-16, 0, -8}
	pause      bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{16, 14, 16}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.map_img = app.ctx.create_image('resources/cubicmap.png') or { gg.Image{} }
	app.atlas = app.ctx.create_image('resources/cubicmap_atlas.png') or { gg.Image{} }
	if app.map_img.data != unsafe { nil } && app.map_img.width > 0 {
		mut cells := []u8{len: app.map_img.width * app.map_img.height}
		for y in 0 .. app.map_img.height {
			for x in 0 .. app.map_img.width {
				cells[y * app.map_img.width + x] = if img_pixel(&app.map_img, x, y).r > 128 { 1 } else { 0 }
			}
		}
		app.mesh = gg.gen_mesh_cubicmap(cells, app.map_img.width, app.map_img.height, 1.0)
		app.model = gg.Model3D{mesh: &app.mesh, pos: app.map_pos}
		app.model.material.texture = &app.atlas
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
	app.ctx.draw_model(&app.model, app.cam)
	app.ctx.end_3d()
	w := f32(app.map_img.width) * 4
	h := f32(app.map_img.height) * 4
	app.ctx.draw_image(800 - w - 20, 20, w, h, &app.map_img)
	app.ctx.draw_rect_empty(800 - w - 20, 20, w, h, cm_green)
	app.ctx.draw_text(658, 90, 'cubicmap image used to', size: 10, color: cm_gray)
	app.ctx.draw_text(658, 104, 'generate map 3d model', size: 10, color: cm_gray)
	app.ctx.end()
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	if !app.pause {
		app.cam.update(.orbital, app.ctx, dt)
	}
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .p {
		app.pause = !app.pause
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - cubicmap rendering'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		bg_color:     cm_raywhite
		depth:        true
	)
	app.ctx.run()
}
