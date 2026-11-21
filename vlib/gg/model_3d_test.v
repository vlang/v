module gg

import os

fn test_model_valid_and_invalid() {
	mesh := gen_mesh_cube(gg.Vec3{1, 1, 1})
	model := gg.Model3D{ mesh: &mesh }
	assert model.valid()
	empty := gg.Model3D{}
	assert !empty.valid()
}

fn test_load_model_from_obj() {
	dir := os.vtmp_dir()
	defer { os.rmdir(dir) or {} }
	path := os.join_path(dir, 'tri.obj')
	os.write_file(path, 'v 0 0 0\nv 1 0 0\nv 0 1 0\nf 1 2 3\n')!
	model := gg.load_model(path)!
	assert model.valid()
	assert model.mesh.vertices.len == 3
}

fn test_draw_model_reports_invalid() {
	mut ctx := gg.new_context_cpu()
	model := gg.Model3D{}
	assert !ctx.draw_model(&model, gg.Camera3D{})
}
