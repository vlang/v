module main

// Port of raylib models_first_person_maze.c
// PORT NOTE: gg's gen_mesh_cubicmap() builds a flat layer of cubes; the maze is
// therefore walled in 2.5D rather than stacked as in raylib. Collision uses the
// cubicmap image pixels just like the original.
import gg
import math

const fpm_raywhite = gg.Color{245, 245, 245, 255}
const fpm_green = gg.Color{0, 255, 0, 255}

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
	ctx      &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	map_img  gg.Image
	atlas    gg.Image
	mesh     gg.Mesh3D
	model    gg.Model3D
	map_pos  gg.Vec3 = gg.Vec3{-16, 0, -8}
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0.2, 0.4, 0.2}
		target:   gg.Vec3{0.185, 0.4, 0}
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
	old := app.cam.position
	app.cam.update(.first_person, app.ctx, 1.0 / 60.0)
	if app.map_img.data != unsafe { nil } && app.map_img.width > 0 {
		cx := int(app.cam.position.x - app.map_pos.x + 0.5)
		cy := int(app.cam.position.z - app.map_pos.z + 0.5)
		if cx < 0 { cx = 0 }
		if cy < 0 { cy = 0 }
		if cx >= app.map_img.width { cx = app.map_img.width - 1 }
		if cy >= app.map_img.height { cy = app.map_img.height - 1 }
		mut collide := false
		mut yy := cy - 1
		for yy <= cy + 1 {
			mut xx := cx - 1
			for xx <= cx + 1 {
				if xx >= 0 && xx < app.map_img.width && yy >= 0 && yy < app.map_img.height {
					if img_pixel(&app.map_img, xx, yy).r == 255 &&
						math.abs(app.cam.position.x - (app.map_pos.x - 0.5 + f32(xx))) < 0.6 &&
						math.abs(app.cam.position.z - (app.map_pos.z - 0.5 + f32(yy))) < 0.6 {
						collide = true
					}
				}
				xx++
			}
			yy++
		}
		if collide {
			app.cam.position = old
		}
	}

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
	x := 800 - w - 20
	app.ctx.draw_image(x, 20, w, h, &app.map_img)
	app.ctx.draw_rect_empty(x, 20, w, h, fpm_green)
	px := x + (app.cam.position.x - app.map_pos.x) * 4
	py := 20 + (app.cam.position.z - app.map_pos.z) * 4
	app.ctx.draw_rect_filled(px, py, 4, 4, gg.red)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - first person maze'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     fpm_raywhite
		depth:        true
	)
	app.ctx.run()
}
