module main

// Port of raylib models_mesh_uv_painting.c
// PORT NOTE: raygui widgets are replaced with plain rectangles/click handling.
// The UV hit test uses gg's ray_triangle() plus inline barycentric interpolation;
// the CPU canvas is uploaded through Image.update_pixel_data() rather than
// UpdateTexture().
import gg
import math

const uv_bg = gg.Color{30, 32, 40, 255}
const uv_gold = gg.Color{255, 203, 40, 255}
const uv_light = gg.Color{211, 211, 211, 255}

const canvas_size = 512

fn barycenter(p gg.Vec3, a gg.Vec3, b gg.Vec3, c gg.Vec3) gg.Vec3 {
	v0 := b - a
	v1 := c - a
	v2 := p - a
	d00 := v0.dot(v0)
	d01 := v0.dot(v1)
	d11 := v1.dot(v1)
	d20 := v2.dot(v0)
	d21 := v2.dot(v1)
	den := d00 * d11 - d01 * d01
	if math.abs(f64(den)) < 1e-6 {
		return gg.Vec3{}
	}
	v := (d11 * d20 - d01 * d21) / den
	w := (d00 * d21 - d01 * d20) / den
	return gg.Vec3{1 - v - w, v, w}
}

fn paint_disc(canvas []u8, cx int, cy int, r int, c gg.Color) {
	for y := cy - r; y <= cy + r; y++ {
		for x := cx - r; x <= cx + r; x++ {
			if x < 0 || y < 0 || x >= canvas_size || y >= canvas_size {
				continue
			}
			if (x - cx) * (x - cx) + (y - cy) * (y - cy) > r * r {
				continue
			}
			i := (y * canvas_size + x) * 4
			unsafe {
				canvas[i] = c.r
				canvas[i + 1] = c.g
				canvas[i + 2] = c.b
				canvas[i + 3] = 255
			}
		}
	}
}

struct App {
mut:
	ctx      &gg.Context = unsafe { nil }
	cam      gg.Camera3D
	mesh     gg.Mesh3D
	model    gg.Model3D
	canvas   []u8
	canvas_img gg.Image
	shape    int
	tool     int
	color    gg.Color = gg.Color{230, 41, 55, 255}
	palette  [8]gg.Color
	brush    int = 12
	has_last bool
	last_x   int
	last_y   int
	orbit    bool
}

fn rebuild_mesh(mut app &App, shape int) {
	if shape == 0 {
		app.mesh = gg.gen_mesh_sphere(1.5, 16, 16)
	} else if shape == 1 {
		app.mesh = gg.gen_mesh_cube(gg.Vec3{2.2, 2.2, 2.2})
	} else if shape == 2 {
		app.mesh = gg.gen_mesh_cylinder(gg.Vec3{0, -1.25, 0}, gg.Vec3{0, 1.25, 0}, 1.2, 16)
	} else {
		app.mesh = gg.gen_mesh_torus(gg.Vec3{}, 1.6, 0.6, 16, 32)
	}
	app.model = gg.Model3D{mesh: &app.mesh}
	app.model.material.texture = &app.canvas_img
	app.shape = shape
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0, 3.5, 6.5}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.canvas = []u8{len: canvas_size * canvas_size * 4}
	for i in 0 .. canvas_size * canvas_size {
		unsafe {
			app.canvas[i * 4] = 245
			app.canvas[i * 4 + 1] = 245
			app.canvas[i * 4 + 2] = 245
			app.canvas[i * 4 + 3] = 255
		}
	}
	app.canvas_img = gg.Image{
		width:       canvas_size
		height:      canvas_size
		nr_channels: 4
		ok:          true
		data:        app.canvas.data
	}
	app.canvas_img.init_sokol_image()
	app.palette = [gg.Color{230, 41, 55, 255}, gg.Color{255, 160, 0, 255},
		gg.Color{255, 203, 40, 255}, gg.Color{0, 255, 0, 255}, gg.Color{135, 206, 235, 255},
		gg.Color{138, 43, 226, 255}, gg.Color{80, 80, 80, 255}, gg.Color{255, 255, 255, 255}]!
	rebuild_mesh(mut app, 0)
}

fn mesh_hit_uv(app &App, ray gg.Ray) ?gg.Vec3 {
	mesh := app.mesh
	mut best := f32(1e9)
	mut result := gg.Vec3{}
	for tri in 0 .. mesh.indices.len / 3 {
		i0 := int(mesh.indices[tri * 3])
		i1 := int(mesh.indices[tri * 3 + 1])
		i2 := int(mesh.indices[tri * 3 + 2])
		if i0 >= mesh.vertices.len || i1 >= mesh.vertices.len || i2 >= mesh.vertices.len {
			continue
		}
		a := mesh.vertices[i0]
		b := mesh.vertices[i1]
		c := mesh.vertices[i2]
		hit := gg.ray_triangle(ray, a, b, c)
		if hit.hit && hit.distance < best {
			best = hit.distance
			w := barycenter(hit.point, a, b, c)
			mut u := w.x * mesh.uvs[i0 * 2] + w.y * mesh.uvs[i1 * 2] + w.z * mesh.uvs[i2 * 2]
			mut v := w.x * mesh.uvs[i0 * 2 + 1] + w.y * mesh.uvs[i1 * 2 + 1] + w.z * mesh.uvs[i2 * 2 + 1]
			u -= f32(math.floor(f64(u)))
			v -= f32(math.floor(f64(v)))
			result = gg.Vec3{u, v, 0}
		}
	}
	if best < 1e9 {
		return result
	}
	return none
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	mx := f32(app.ctx.mouse_pos_x)
	my := f32(app.ctx.mouse_pos_y)
	if app.orbit {
		app.cam.update(.free, app.ctx, 1.0 / 60.0)
	}
	if mx > 240 && app.ctx.mouse_buttons.has(.left) {
		ray := gg.screen_to_world_ray(mx, my, app.cam, 800, 450)
		if uv := mesh_hit_uv(app, ray) {
			px := int(uv.x * canvas_size)
			py := int(uv.y * canvas_size)
			if app.tool == 1 {
				if px >= 0 && py >= 0 && px < canvas_size && py < canvas_size {
					i := (py * canvas_size + px) * 4
					app.color = unsafe { gg.Color{app.canvas[i], app.canvas[i + 1], app.canvas[i + 2], 255} }
				}
				app.tool = 0
				app.has_last = false
			} else {
				if app.has_last {
					steps := int(math.sqrt(f64((px - app.last_x) * (px - app.last_x) +
						(py - app.last_y) * (py - app.last_y)))) / 2 + 1
					for k in 0 .. steps {
						t := f32(k) / f32(steps)
						x := int(f32(app.last_x) + (f32(px) - f32(app.last_x)) * t)
						y := int(f32(app.last_y) + (f32(py) - f32(app.last_y)) * t)
						paint_disc(app.canvas, x, y, app.brush, app.color)
					}
				} else {
					paint_disc(app.canvas, px, py, app.brush, app.color)
				}
				app.last_x = px
				app.last_y = py
				app.has_last = true
			}
		} else {
			app.has_last = false
		}
	} else {
		app.has_last = false
	}
	app.canvas_img.update_pixel_data(app.canvas.data)

	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_model(&app.model, app.cam)
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_rect_filled(10, 10, 230, 430, gg.Color{0, 0, 0, 204})
	app.ctx.draw_rect_empty(10, 10, 230, 430, gg.Color{80, 80, 80, 255})
	app.ctx.draw_text(25, 22, 'MESH UV PAINTER', size: 20, color: uv_gold)
	app.ctx.draw_text(25, 100, 'Shape: 1 sphere 2 cube 3 cylinder 4 torus', size: 10, color: uv_light)
	app.ctx.draw_text(25, 115, 'Tool: P paint  I picker  (no raygui)', size: 10, color: uv_light)
	app.ctx.draw_text(25, 195, 'Active Color:', size: 10, color: uv_light)
	app.ctx.draw_rect_filled(125, 193, 95, 20, app.color)
	for i in 0 .. 8 {
		r := gg.Rect{25 + f32(i % 4) * 48, 245 + f32(i / 4) * 45, 40, 38}
		app.ctx.draw_rect_filled(r.x, r.y, r.width, r.height, app.palette[i])
		if mx >= r.x && mx <= r.x + r.width && my >= r.y && my <= r.y + r.height &&
			app.ctx.mouse_buttons.has(.left) {
			app.color = app.palette[i]
		}
	}
	app.ctx.draw_text(25, 345, 'Brush Size: ${app.brush}px (+/-)', size: 10, color: uv_light)
	app.ctx.draw_text(25, 410, 'C: clear  F: fill  RMB: orbit', size: 10, color: uv_light)
	app.ctx.end()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .mouse_button_down || e.typ == .mouse_button_up {
		app.orbit = app.ctx.mouse_buttons.has(.right)
	}
	if e.typ != .key_down {
		return
	}
	match e.key_code {
		.p { app.tool = 0 }
		.i { app.tool = 1 }
		.c {
			for i in 0 .. canvas_size * canvas_size {
				unsafe {
					app.canvas[i * 4] = 245
					app.canvas[i * 4 + 1] = 245
					app.canvas[i * 4 + 2] = 245
					app.canvas[i * 4 + 3] = 255
				}
			}
		}
		.f {
			for i in 0 .. canvas_size * canvas_size {
				unsafe {
					app.canvas[i * 4] = app.color.r
					app.canvas[i * 4 + 1] = app.color.g
					app.canvas[i * 4 + 2] = app.color.b
					app.canvas[i * 4 + 3] = 255
				}
			}
		}
		.equal { app.brush += 2 }
		.minus {
			app.brush -= 2
			if app.brush < 2 { app.brush = 2 }
		}
		else {}
	}
	if e.key_code == gg.KeyCode._1 { rebuild_mesh(mut app, 0) }
	if e.key_code == gg.KeyCode._2 { rebuild_mesh(mut app, 1) }
	if e.key_code == gg.KeyCode._3 { rebuild_mesh(mut app, 2) }
	if e.key_code == gg.KeyCode._4 { rebuild_mesh(mut app, 3) }
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - mesh uv painting'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		event_fn:     on_event
		bg_color:     uv_bg
		depth:        true
	)
	app.ctx.run()
}
