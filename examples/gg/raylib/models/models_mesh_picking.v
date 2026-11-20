module main

// Port of raylib models_mesh_picking.c
// PORT NOTE: gg's ray helpers cover sphere/box/triangle/mesh; quad picking is
// expressed as two triangles, and barycentric coordinates are computed inline.
import gg
import math

const mp_raywhite = gg.Color{245, 245, 245, 255}
const mp_purple = gg.Color{138, 43, 226, 255}
const mp_orange = gg.Color{255, 160, 0, 255}
const mp_green = gg.Color{0, 255, 0, 255}
const mp_maroon = gg.Color{128, 0, 0, 255}

struct App {
mut:
	ctx          &gg.Context = unsafe { nil }
	cam          gg.Camera3D
	model        gg.Model3D
	has_model    bool
	first_person bool
	hit_name     string
	hit          gg.RayHit
	hit_color    gg.Color
	bary         gg.Vec3
	tri_hit      gg.RayHit
	sp           gg.Vec3 = gg.Vec3{-30, 5, 5}
	sr           f32     = 4
	ta           gg.Vec3 = gg.Vec3{-25, 0.5, 0}
	tb           gg.Vec3 = gg.Vec3{-4, 2.5, 1}
	tc           gg.Vec3 = gg.Vec3{-8, 6.5, 0}
}

fn barycenter(p gg.Vec3, a gg.Vec3, b gg.Vec3, c gg.Vec3) gg.Vec3 {
	v0 := b - a
	v1 := c - a
	v2 := p - a
	d00 := v0.dot(v0)
	d01 := v0.dot(v1)
	d11 := v1.dot(v1)
	d20 := v2.dot(v0)
	d21 := v2.dot(v1)
	denom := d00 * d11 - d01 * d01
	if math.abs(f64(denom)) < 1e-6 {
		return gg.Vec3{}
	}
	v := (d11 * d20 - d01 * d21) / denom
	w := (d00 * d21 - d01 * d20) / denom
	return gg.Vec3{1 - v - w, v, w}
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{20, 20, 20}
		target:   gg.Vec3{0, 8, 0}
		fovy:     45
	}
	app.model = gg.load_model('resources/models/obj/turret.obj') or { gg.Model3D{} }
	app.has_model = app.model.valid()
	tex := app.ctx.create_image('resources/models/obj/turret_diffuse.png') or { gg.Image{} }
	if app.has_model {
		app.model.material.texture = &tex
	}
}

fn closest(candidate gg.RayHit, name string, color gg.Color, mut best &gg.RayHit, mut best_name &string, mut best_color &gg.Color) {
	if candidate.hit && (!best.hit || candidate.distance < best.distance) {
		best.hit = candidate.hit
		best.distance = candidate.distance
		best.point = candidate.point
		// Dereferenced writes require an unsafe block.
		unsafe {
			*best_name = name
			*best_color = color
		}
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.mouse_buttons.has(.right) {
		app.first_person = !app.first_person
	}
	ray := gg.screen_to_world_ray(f32(app.ctx.mouse_pos_x), f32(app.ctx.mouse_pos_y), app.cam, 800, 450)
	app.hit = gg.RayHit{}
	app.hit_name = 'None'
	app.hit_color = gg.white

	// Ground quad as two triangles.
	g0 := gg.Vec3{-50, 0, -50}
	g1 := gg.Vec3{-50, 0, 50}
	g2 := gg.Vec3{50, 0, 50}
	g3 := gg.Vec3{50, 0, -50}
	mut gnd := gg.ray_triangle(ray, g0, g1, g2)
	if !gnd.hit {
		gnd = gg.ray_triangle(ray, g0, g2, g3)
	}
	closest(gnd, 'Ground', mp_green, mut &app.hit, mut &app.hit_name, mut &app.hit_color)

	app.tri_hit = gg.ray_triangle(ray, app.ta, app.tb, app.tc)
	closest(app.tri_hit, 'Triangle', mp_purple, mut &app.hit, mut &app.hit_name, mut &app.hit_color)
	if app.tri_hit.hit && app.hit_name == 'Triangle' {
		app.bary = barycenter(app.tri_hit.point, app.ta, app.tb, app.tc)
	}

	sph := gg.ray_sphere(ray, app.sp, app.sr)
	closest(sph, 'Sphere', mp_orange, mut &app.hit, mut &app.hit_name, mut &app.hit_color)

	if app.has_model {
		mesh_hit := gg.ray_mesh(ray, app.model.mesh, app.model.pos, app.model.rotation, 1.0)
		closest(mesh_hit, 'Mesh', mp_orange, mut &app.hit, mut &app.hit_name, mut &app.hit_color)
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
	app.ctx.draw_line_3d(app.ta, app.tb, mp_purple)
	app.ctx.draw_line_3d(app.tb, app.tc, mp_purple)
	app.ctx.draw_line_3d(app.tc, app.ta, mp_purple)
	app.ctx.draw_sphere_wires(app.sp, app.sr, 8, 8, mp_purple)
	if app.hit.hit {
		app.ctx.draw_cube(app.hit.point, gg.Vec3{0.3, 0.3, 0.3}, app.hit_color)
		app.ctx.draw_cube_wires(app.hit.point, gg.Vec3{0.3, 0.3, 0.3}, gg.red)
		app.ctx.draw_line_3d(app.hit.point, app.hit.point + gg.Vec3{0, 1, 0}, gg.red)
	}
	app.ctx.draw_ray(ray, 100, mp_maroon)
	app.ctx.draw_grid(10, 10.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(10, 50, 'Hit Object: ${app.hit_name}', size: 10, color: mp_black())
	if app.hit.hit {
		app.ctx.draw_text(10, 70, 'Distance: ${app.hit.distance:.2f}', size: 10, color: mp_black())
		p := app.hit.point
		app.ctx.draw_text(10, 85, 'Hit Pos: ${p.x:.2f} ${p.y:.2f} ${p.z:.2f}',
			size:  10
			color: mp_black()
		)
		if app.hit_name == 'Triangle' {
			app.ctx.draw_text(10, 100,
				'Barycenter: ${app.bary.x:.2f} ${app.bary.y:.2f} ${app.bary.z:.2f}',
				size:  10
				color: mp_black()
			)
		}
	}
	app.ctx.draw_text(10, 430, 'Right click mouse to toggle camera controls',
		size:  10
		color: gg.gray
	)
	app.ctx.end()
}

fn mp_black() gg.Color {
	return gg.Color{0, 0, 0, 255}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - mesh picking'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     mp_raywhite
		depth:        true
	)
	app.ctx.run()
}
