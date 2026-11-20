module main

// Port of raylib models_heightmap_rendering.c
// PORT NOTE: gg's gen_mesh_heightmap() takes a row-major height array rather
// than an image; the red channel of heightmap.png is converted into that array.
import gg

const hm_raywhite = gg.Color{245, 245, 245, 255}
const hm_green = gg.Color{0, 255, 0, 255}

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
	ctx     &gg.Context = unsafe { nil }
	cam     gg.Camera3D
	img     gg.Image
	mesh    gg.Mesh3D
	model   gg.Model3D
	map_pos gg.Vec3 = gg.Vec3{-8, 0, -8}
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{18, 21, 18}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.img = app.ctx.create_image('resources/heightmap.png') or { gg.Image{} }
	if app.img.data != unsafe { nil } && app.img.width > 1 {
		mut heights := []f32{len: app.img.width * app.img.height}
		for y in 0 .. app.img.height {
			for x in 0 .. app.img.width {
				heights[y * app.img.width + x] = f32(img_pixel(&app.img, x, y).r) / 255.0 * 8.0
			}
		}
		// Spacing of 1.0 keeps the mesh roughly 16x16 like raylib's (16,8,16) size.
		app.mesh = gg.gen_mesh_heightmap(heights, app.img.width, app.img.height, 1.0)
		app.model = gg.Model3D{mesh: &app.mesh, pos: app.map_pos}
		app.model.material.texture = &app.img
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
	app.ctx.draw_grid(20, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_image(800 - f32(app.img.width) - 20, 20, f32(app.img.width), f32(app.img.height), &app.img)
	app.ctx.draw_rect_empty(800 - f32(app.img.width) - 20, 20, f32(app.img.width), f32(app.img.height), hm_green)
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
		window_title: 'raylib [models] example - heightmap rendering'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		update_fn:    update
		bg_color:     hm_raywhite
		depth:        true
	)
	app.ctx.run()
}
