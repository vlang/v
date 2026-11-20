module main

// Port of raylib models_basic_voxel.c
// PORT NOTE: gg has no GetRayCollisionBox()/GetScreenToWorldRay() pair, but the
// equivalent gg.ray_box() + gg.screen_to_world_ray() helpers are used here.
import gg

const bv_raywhite = gg.Color{245, 245, 245, 255}
const bv_beige = gg.Color{245, 245, 220, 255}
const bv_black = gg.Color{0, 0, 0, 255}
const bv_red = gg.Color{255, 0, 0, 255}
const bv_darkgray = gg.Color{80, 80, 80, 255}

const world_size = 8

struct App {
mut:
	ctx    &gg.Context = unsafe { nil }
	cam    gg.Camera3D
	mesh   gg.Mesh3D
	model  gg.Model3D
	voxels [world_size][world_size][world_size]bool
}

fn init(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam = gg.Camera3D{
		position: gg.Vec3{-2, 0, -2}
		target:   gg.Vec3{}
		fovy:     45
	}
	app.mesh = gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	app.model = gg.Model3D{mesh: &app.mesh}
	app.model.material.color = bv_beige
	for x in 0 .. world_size {
		for y in 0 .. world_size {
			for z in 0 .. world_size {
				app.voxels[x][y][z] = true
			}
		}
	}
}

fn frame(data voidptr) {
	mut app := unsafe { &App(data) }
	app.cam.update(.first_person, app.ctx, 1.0 / 60.0)
	if app.ctx.mouse_buttons.has(.left) {
		ray := gg.screen_to_world_ray(400, 225, app.cam, 800, 450)
		mut best := f32(1e9)
		mut bx := -1
		mut by := -1
		mut bz := -1
		for x in 0 .. world_size {
			for y in 0 .. world_size {
				for z in 0 .. world_size {
					if !app.voxels[x][y][z] {
						continue
					}
					hit := gg.ray_box(ray, gg.Vec3{f32(x), f32(y), f32(z)}, gg.Vec3{1, 1, 1})
					if hit.hit && hit.distance < best {
						best = hit.distance
						bx = x
						by = y
						bz = z
					}
				}
			}
		}
		if bx >= 0 {
			app.voxels[bx][by][bz] = false
		}
	}

	app.ctx.begin()
	app.ctx.set_camera(app.cam)
	app.ctx.begin_3d(cam: app.cam) or {
		app.ctx.end()
		return
	}
	app.ctx.draw_grid(10, 1.0, gg.gray)
	for x in 0 .. world_size {
		for y in 0 .. world_size {
			for z in 0 .. world_size {
				if app.voxels[x][y][z] {
					pos := gg.Vec3{f32(x), f32(y), f32(z)}
					app.model.pos = pos
					app.ctx.draw_model(&app.model, app.cam)
					app.ctx.draw_cube_wires(pos, gg.Vec3{1, 1, 1}, bv_black)
				}
			}
		}
	}
	app.ctx.end_3d()
	app.ctx.draw_circle_filled(400, 225, 4, bv_red)
	app.ctx.draw_text(10, 10, 'Left-click a voxel to remove it!', size: 20, color: bv_darkgray)
	app.ctx.draw_text(10, 35, 'WASD to move, mouse to look around', size: 10, color: bv_beige)
	app.ctx.end()
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [models] example - basic voxel'
		user_data:    app
		init_fn:      init
		frame_fn:     frame
		bg_color:     bv_raywhite
		depth:        true
	)
	app.ctx.run()
}
