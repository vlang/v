// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// draw_model draws a Model3D using its stored transform and material.
// It returns false when the model has no valid mesh.
pub fn (ctx &Context) draw_model(m &Model3D, cam Camera3D) bool {
	if !m.valid() {
		return false
	}
	return ctx.draw_material_mesh_s(m.mesh, m.pos, m.rotation, m.scale, &m.material, cam)
}
