module main

import gg

fn main() {
	mut ctx := gg.new_context_cpu()
	mut cam := gg.new_camera3d(gg.Vec3{0, 4, 9}, gg.Vec3{})
	cam.orbit(35, 20, 9)
	mut mesh := gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	mut model := gg.Model3D{
		mesh:     &mesh
		material: gg.Material3D{ color: gg.green, ambient: 0.4 }
		pos:      gg.Vec3{0, 1, 0}
		rotation: gg.Vec3{0, 30, 0}
		scale:    gg.Vec3{1.5, 1, 1}
	}
	ctx.debug_mode3d(true, true)
	ctx.draw_model(&model, cam)
	ctx.draw_aabb(gg.Vec3{0, 1, 0}, gg.Vec3{2, 2, 2}, gg.yellow)
	frame := ctx.render_cpu_3d(160, 120, gg.black, cam)
	frame.save_ppm('gg_debug_3d.ppm') or { panic(err.msg()) }
}
