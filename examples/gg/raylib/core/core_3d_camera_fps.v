module main

// raylib [core] example - 3d camera fps
// A first-person controller with gravity, jumping, crouching and a walking
// head bob. PORT NOTE: raylib's full strafing/air-drag model is simplified
// here to acceleration toward the looked direction, but the movement and
// scene are otherwise preserved.

import gg
import math
import sokol.sapp

const screen_width = 800
const screen_height = 450
const gravity = f32(32)
const max_speed = f32(20)
const crouch_speed = f32(5)
const jump_force = f32(12)
const max_accel = f32(150)
const friction = f32(0.86)
const air_drag = f32(0.98)
const control = f32(15)
const stand_height = f32(1)
const bottom_height = f32(0.5)

const ray_white = gg.Color{245, 245, 245, 255}
const sky_blue = gg.Color{102, 191, 255, 128}
const blue = gg.Color{0, 121, 241, 255}
const black = gg.Color{0, 0, 0, 255}
const dark_blue = gg.Color{0, 82, 172, 255}
const tile_color = gg.Color{150, 200, 200, 255}

struct App {
mut:
	gg         &gg.Context = unsafe { nil }
	cam        gg.Camera3D = gg.Camera3D{ position: gg.Vec3{0, 1.5, 0}, target: gg.Vec3{0, 1.5, -1} }
	position   gg.Vec3
	velocity   gg.Vec3
	look_yaw   f32
	look_pitch f32
	grounded   bool
	head_timer f32
	walk_lerp  f32
	head_lerp  f32 = stand_height
	lean       gg.Vec2
}

fn main() {
	mut app := &App{}
	app.cam.fovy = 60
	app.cam.position = gg.Vec3{0, bottom_height + app.head_lerp, 0}
	sapp.lock_mouse(true)
	app.gg = gg.new_context(
		width:        screen_width
		height:       screen_height
		window_title: 'raylib [core] example - 3d camera fps'
		bg_color:     ray_white
		depth:        true
		frame_fn:     frame
		update_fn:    update
		event_fn:     on_event
		user_data:    app
	)
	app.gg.run()
}

fn on_event(e &gg.Event, data voidptr) {
	mut app := unsafe { &App(data) }
	if e.typ == .key_down && e.key_code == .space && !e.key_repeat {
		if app.grounded {
			app.velocity.y = jump_force
			app.grounded = false
		}
	}
}

fn update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.gg

	app.look_yaw -= f32(ctx.mouse_dx) * 0.001
	app.look_pitch += f32(ctx.mouse_dy) * 0.001
	if app.look_pitch > 1.5 {
		app.look_pitch = 1.5
	}
	if app.look_pitch < -1.5 {
		app.look_pitch = -1.5
	}

	// Mouse look is incremental; cam.fly rotates around the view axes.
	app.cam.fly(0, 0, 0, -f32(ctx.mouse_dx) * 0.3, f32(ctx.mouse_dy) * 0.3)

	mut side := 0
	mut forward := 0
	if ctx.pressed_keys[int(gg.KeyCode.d)] {
		side += 1
	}
	if ctx.pressed_keys[int(gg.KeyCode.a)] {
		side -= 1
	}
	if ctx.pressed_keys[int(gg.KeyCode.w)] {
		forward += 1
	}
	if ctx.pressed_keys[int(gg.KeyCode.s)] {
		forward -= 1
	}
	crouching := ctx.pressed_keys[int(gg.KeyCode.left_control)]

	if !app.grounded {
		app.velocity.y -= gravity * dt
	}
	if app.grounded {
		app.velocity.y = 0
	}

	// Accelerate horizontal velocity toward the looked direction.
	fwd := app.cam.forward()
	right := app.cam.right()
	mut dir := fwd.scale(f32(forward)) + right.scale(f32(side))
	if dir.dot(dir) > 0 {
		dir = dir.normalized()
	}
	decel := if app.grounded { friction } else { air_drag }
	app.velocity.x *= decel
	app.velocity.z *= decel
	app.velocity.x += dir.x * max_accel * dt
	app.velocity.z += dir.z * max_accel * dt
	top := if crouching { crouch_speed } else { max_speed }
	speed := f32(math.sqrt(f64(app.velocity.x * app.velocity.x + app.velocity.z * app.velocity.z)))
	if speed > top {
		k := top / speed
		app.velocity.x *= k
		app.velocity.z *= k
	}

	app.position = app.position + app.velocity.scale(dt)
	if app.position.y <= 0 {
		app.position.y = 0
		app.velocity.y = 0
		app.grounded = true
	}

	head_target := if crouching { 0.0 } else { stand_height }
	app.head_lerp += (head_target - app.head_lerp) * 20 * dt
	if app.grounded && (forward != 0 || side != 0) {
		app.head_timer += dt * 3
		app.walk_lerp += (1 - app.walk_lerp) * 10 * dt
		app.cam.fovy += (55 - app.cam.fovy) * 5 * dt
	} else {
		app.walk_lerp += (0 - app.walk_lerp) * 10 * dt
		app.cam.fovy += (60 - app.cam.fovy) * 5 * dt
	}
	app.lean.x += (f32(side) * 0.02 - app.lean.x) * 10 * dt
	app.lean.y += (f32(forward) * 0.015 - app.lean.y) * 10 * dt

	app.cam.position = gg.Vec3{app.position.x, app.position.y + bottom_height + app.head_lerp, app.position.z}
	app.cam.up = gg.Vec3{0, 1, 0}
	app.cam.position.y += math.abs(math.cosf(app.head_timer * f32(math.pi))) * 0.15 * app.walk_lerp
	app.cam.target = app.cam.position + app.cam.forward()
}

fn frame(data voidptr) {
	app := unsafe { &App(data) }
	ctx := app.gg
	ctx.begin()
	ctx.set_camera(app.cam)
	ctx.begin_3d() or { panic(err.msg()) }
	for y in -25 .. 25 {
		for x in -25 .. 25 {
			if y % 2 == 0 && x % 2 == 0 {
				ctx.draw_plane(gg.Vec3{f32(x) * 5, 0, f32(y) * 5}, 5, 5, gg.light_gray)
			} else if y % 2 != 0 && x % 2 != 0 {
				ctx.draw_plane(gg.Vec3{f32(x) * 5, 0, f32(y) * 5}, 5, 5, tile_color)
			}
		}
	}
	tower_size := gg.Vec3{16, 32, 16}
	for sx in [-1, 1] {
		for sz in [-1, 1] {
			pos := gg.Vec3{f32(sx) * 16, 16, f32(sz) * 16}
			ctx.draw_cube(pos, tower_size, tile_color)
			ctx.draw_cube_wires(pos, tower_size, dark_blue)
		}
	}
	ctx.draw_sphere(gg.Vec3{300, 300, 0}, 100, 12, 24, gg.Color{255, 0, 0, 255})
	ctx.end_3d()

	ctx.draw_rect(x: 5, y: 5, w: 330, h: 75, color: sky_blue)
	ctx.draw_rect_empty(5, 5, 330, 75, blue)
	ctx.draw_text(15, 15, 'Camera controls:', size: 10, color: black)
	ctx.draw_text(15, 30, '- Move keys: W, A, S, D, Space, Left-Ctrl', size: 10, color: black)
	ctx.draw_text(15, 45, '- Look around: arrow keys or mouse', size: 10, color: black)
	ctx.draw_text(15, 60,
		'- Velocity Len: ${f32(math.sqrt(f64(app.velocity.x * app.velocity.x + app.velocity.z * app.velocity.z))):.3f}',
		size:  10
		color: black
	)
	ctx.end()
}
