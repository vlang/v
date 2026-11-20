// Render a small 3D scene entirely on the CPU, with no window or GPU.
// Run with: v run examples/gg/3d_cpu.v
module main

import gg

fn main() {
	mut ctx := gg.new_context_cpu()
	cam := gg.Camera3D{
		position: gg.Vec3{0, 2, 6}
		target:   gg.Vec3{0, 0, 0}
	}
	mesh := gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	mut mat := gg.Material3D{
		color:     gg.red
		ambient:   0.3
		light:     gg.Light3D{ direction: gg.Vec3{0.5, -1, 0.25} }
		fog_color: gg.black
		fog_near:  5
		fog_far:   20
	}
	ctx.draw_material_mesh(&mesh, gg.Vec3{-1.2, 0, 0}, gg.Vec3{0, 20, 0}, 1.0, &mat, cam)
	mat.color = gg.blue
	ctx.draw_material_mesh(&mesh, gg.Vec3{1.2, 0, 0}, gg.Vec3{0, -15, 0}, 1.0, &mat, cam)
	frame := ctx.render_cpu_3d(160, 120, gg.rgb(20, 24, 32), cam)
	frame.save_ppm('gg_cpu_3d.ppm')
	println('wrote gg_cpu_3d.ppm ${frame.width}x${frame.height}, depth samples=${frame.depth.len}')
}
