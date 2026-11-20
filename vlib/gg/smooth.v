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
	// Pack the three quantized bucket coords into a single u64 key. Each coord
	// is masked to 20 bits; this avoids the per-vertex string allocation of the
	// old map[string][]int while keeping equal positions in the same group.
	mut groups := map[u64][]int{}
	for i, v in mesh.vertices {
		// Round to nearest bucket so positions just across a bucket edge
		// still merge; truncation would split them by up to one precision.
		x := int(v.x * inv_p + 0.5)
		y := int(v.y * inv_p + 0.5)
		z := int(v.z * inv_p + 0.5)
		key := (u64(x & 0xfffff) << 40) | (u64(y & 0xfffff) << 20) | u64(z & 0xfffff)
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
			inv_len := 1.0 / f32(math.sqrt(len_sq))
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
