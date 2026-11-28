// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import math

// smooth_normals averages normals of vertices that share a position, giving
// smooth (Gouraud) shading instead of flat faceted shading. It returns a new
// normal slice; the mesh itself is unchanged. Positions are quantized to a
// grid so floating-point differences do not hide shared vertices.
pub fn smooth_normals(mesh &Mesh3D, precision f32) []Vec3 {
	p := if precision > 0 { precision } else { 1e-3 }
	inv_p := 1.0 / p
	// Use an exact string key: bit-packing into u64 aliases distinct bucket
	// coordinates (negative values and values beyond the packed width), which
	// averages normals of unrelated vertices. Correctness outweighs the small
	// allocation saved by packing.
	mut groups := map[string][]int{}
	for i, v in mesh.vertices {
		// Round to nearest bucket so positions just across a bucket edge
		// still merge; truncation would split them by up to one precision.
		x := int(v.x * inv_p + 0.5)
		y := int(v.y * inv_p + 0.5)
		z := int(v.z * inv_p + 0.5)
		key := '${x},${y},${z}'
		groups[key] << i
	}
	mut out := []Vec3{len: mesh.vertices.len}
	for _, ids in groups {
		mut sum := Vec3{}
		for i in ids {
			if i < mesh.normals.len {
				sum += mesh.normals[i]
			}
		}
		// Normalize inline using math.sqrt on the squared length, matching
		// Vec3.normalized(): a zero vector stays zero. This avoids the extra
		// scale() call on the hot path.
		len_sq := sum.x * sum.x + sum.y * sum.y + sum.z * sum.z
		mut n := Vec3{}
		if len_sq != 0 {
			inv_len := 1.0 / math.sqrtf(len_sq)
			n = Vec3{sum.x * inv_len, sum.y * inv_len, sum.z * inv_len}
		}
		for i in ids {
			out[i] = n
		}
	}
	return out
}

// smooth_mesh returns a copy of mesh whose normals are averaged across
// shared vertices, producing smooth shading.
pub fn smooth_mesh(mesh &Mesh3D) Mesh3D {
	mut m := Mesh3D{
		vertices: mesh.vertices.clone()
		normals:  smooth_normals(mesh, 1e-3)
		uvs:      mesh.uvs.clone()
		indices:  mesh.indices.clone()
	}
	return m
}

// smooth_mesh_precision is smooth_mesh with a caller-supplied merge precision.
pub fn smooth_mesh_precision(mesh &Mesh3D, precision f32) Mesh3D {
	return Mesh3D{
		vertices: mesh.vertices.clone()
		normals:  smooth_normals(mesh, precision)
		uvs:      mesh.uvs.clone()
		indices:  mesh.indices.clone()
	}
}
