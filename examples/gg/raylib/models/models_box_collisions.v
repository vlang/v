module main

// Port of raylib models_box_collisions.c
// PORT NOTE: gg has no CheckCollisionBoxes()/CheckCollisionBoxSphere(); simple
// AABB/AABB and AABB/sphere tests are implemented directly here.
import gg
import math

const bc_raywhite = gg.Color{245, 245, 245, 255}
const bc_gray = gg.Color{128, 128, 128, 255}
const bc_darkgray = gg.Color{80, 80, 80, 255}

fn boxes_overlap(a_center gg.Vec3, a_size gg.Vec3, b_center gg.Vec3, b_size gg.Vec3) bool {
	return math.abs(a_center.x - b_center.x) * 2 < (a_size.x + b_size.x) &&
		math.abs(a_center.y - b_center.y) * 2 < (a_size.y + b_size.y) &&
		math.abs(a_center.z - b_center.z) * 2 < (a_size.z + b_size.z)
}

fn box_sphere(center gg.Vec3, size gg.Vec3, sphere gg.Vec3, radius f32) bool {
	closest := gg.Vec3{
		x: math.max(f64(center.x - size.x / 2), math.min(f64(sphere.x), f64(center.x + size.x / 2)))
		y: math.max(f64(center.y - size.y / 2), math.min(f64(sphere.y), f64(center.y + size.y / 2)))
		z: math.max(f64(center.z - size.z / 2), math.min(f64(sphere.z), f64(center.z + size.z / 2)))
	}
	d := sphere - closest
	return d.dot(d) <= radius * radius
}

struct App {
mut:
	ctx           &gg.Context = unsafe { nil }
	cam           gg.Camera3D
	player        gg.Vec3
	player_size   gg.Vec3 = gg.Vec3{1, 2, 1}
	enemy_box     gg.Vec3 = gg.Vec3{-4, 1, 0}
	enemy_size    gg.Vec3 = gg.Vec3{2, 2, 2}
	enemy_sphere  gg.Vec3 = gg.Vec3{4, 0, 0}
	sphere_radius f32 = 1.5
	collision     bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{0, 10, 10}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.player = gg.Vec3{0, 1, 2}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	if app.ctx.pressed_keys[gg.KeyCode.right] {
		app.player.x += 0.2
	} else if app.ctx.pressed_keys[gg.KeyCode.left] {
		app.player.x -= 0.2
	} else if app.ctx.pressed_keys[gg.KeyCode.down] {
		app.player.z += 0.2
	} else if app.ctx.pressed_keys[gg.KeyCode.up] {
		app.player.z -= 0.2
	}
	app.collision = boxes_overlap(app.player, app.player_size, app.enemy_box, app.enemy_size) ||
		box_sphere(app.player, app.player_size, app.enemy_sphere, app.sphere_radius)
	player_color := if app.collision { gg.red } else { gg.green }

	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_cube(app.enemy_box, app.enemy_size, bc_gray)
	app.ctx.draw_cube_wires(app.enemy_box, app.enemy_size, bc_darkgray)
	app.ctx.draw_sphere(app.enemy_sphere, app.sphere_radius, 16, 16, bc_gray)
	app.ctx.draw_sphere_wires(app.enemy_sphere, app.sphere_radius, 16, 16, bc_darkgray)
	app.ctx.draw_cube(app.player, app.player_size, player_color)
	app.ctx.draw_grid(10, 1.0, gg.gray)
	app.ctx.end_3d()
	app.ctx.draw_text(220, 40, 'Move player with arrow keys to collide', size: 20, color: bc_gray)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - box collisions'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     bc_raywhite
		depth:        true
	)
	app.ctx.run()
}
