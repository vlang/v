// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

// Model3D combines geometry, appearance and a transform in one value.
// It is the simple alternative to re-passing mesh/material/position on every
// draw call; see raylib's Model and three.js's Mesh.
pub struct Model3D {
pub mut:
	mesh     &Mesh3D = unsafe { nil }
	material Material3D
	pos      Vec3
	rotation Vec3
	scale    Vec3 = Vec3{1, 1, 1}
}

// valid reports whether the model has usable geometry.
pub fn (m &Model3D) valid() bool {
	return !isnil(m.mesh) && m.mesh.valid()
}
