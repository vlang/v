// Port of raylib [shaders] example - custom uniform (swirl post-process).
// PORT NOTE: shader swirl.fs with a custom "center" uniform requires gg user-shader support
// (not yet available); the swirl is applied on the CPU to a RenderTexture2D-sized image.
module main

import gg
import math

const win_width = 800
const win_height = 450
const img_w = 400
const img_h = 225

struct App {
mut:
	gg       &gg.Context = unsafe { nil }
	image_id int         = -1
	pixels   []u8
	cam      gg.Camera3D
	mesh     gg.Mesh3D
	cx       f32 = 400
	cy       f32 = 225
	mouse_x  f32 = 400
	mouse_y  f32 = 225
	angle    f32
}

fn main() {
	mut app := &App{}
	app.pixels = []u8{len: img_w * img_h * 4, init: 255}
	app.cam = gg.Camera3D{ position: gg.Vec3{8, 8, 8}, target: gg.Vec3{0, 1.5, 0}, fovy: 45 }
	app.cam.orbit(30, 20, 12)
	app.mesh = gg.gen_mesh_cube(gg.Vec3{2, 2, 2})
	app.gg = gg.new_context(
		width:         win_width
		height:        win_height
		window_title:  'raylib [shaders] example - custom uniform'
		bg_color:      gg.white
		depth:         true
		user_data:     app
		init_fn:       init
		update_fn:     update
		event_fn:      event
		frame_fn:      frame
		create_window: true
	)
	app.gg.run()
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.image_id = app.gg.new_streaming_image(img_w, img_h, 4, gg.StreamingImageConfig{})
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.orbital, app.gg, dt)
	app.angle += dt * 20
}

fn event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	app.mouse_x = e.mouse_x
	app.mouse_y = e.mouse_y
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	// CPU swirl warp of a procedural scene around the "center" uniform.
	for y in 0 .. img_h {
		for x in 0 .. img_w {
			dx := f32(x) - app.mouse_x / 2
			dy := f32(y) - app.mouse_y / 2
			r := math.sqrt(dx * dx + dy * dy)
			a := math.atan2(dy, dx) + 2.5 / (1 + r * 0.05)
			mut sx := int(app.mouse_x / 2 + f32(math.cos(f64(a))) * r)
			mut sy := int(app.mouse_y / 2 + f32(math.sin(f64(a))) * r)
			if sx < 0 || sx >= img_w || sy < 0 || sy >= img_h {
				sx, sy = x, y
			}
			at := (y * img_w + x) * 4
			app.pixels[at] = u8((sx * 255 / img_w))
			app.pixels[at + 1] = u8((sy * 255 / img_h))
			app.pixels[at + 2] = u8(120 + 100 * math.sinf(f32(sx) * 0.02 + app.angle * 0.01))
			app.pixels[at + 3] = 255
		}
	}
	app.gg.update_pixel_data(app.image_id, app.pixels.data)
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	mat := gg.Material3D{ color: gg.rgb(180, 170, 150), ambient: 0.5 }
	ctx.draw_material_mesh(&app.mesh, gg.Vec3{}, gg.Vec3{}, 0.5, &mat, app.cam)
	ctx.draw_grid(10, 1, gg.gray)
	ctx.end_3d()
	ctx.draw_text(10, 10, 'Swirl shader approximated on CPU (RenderTexture-sized image)',
		size:  14
		color: gg.dark_gray
	)
	ctx.end()
}
