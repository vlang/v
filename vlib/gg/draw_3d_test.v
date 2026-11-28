module gg

import math

fn test_cube_geometry_counts() {
	assert cube_triangles.len == 36
	assert cube_edges.len == 24
	corners := cube_corners(Vec3{0, 0, 0}, Vec3{2, 2, 2})
	assert corners.len == 8
	assert corners[0].x == -1 && corners[7].z == 1
}

fn test_sphere_triangles_have_outward_winding() {
	verts := sphere_triangles(Vec3{0, 0, 0}, 2.0, 6, 12)
	assert verts.len == 6 * 12 * 6
	for v in verts {
		len := math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z)
		assert math.abs(len - 2.0) < 1e-4
	}
	// The first triangle's normal should point away from the centre.
	a := verts[0]
	b := verts[1]
	c := verts[2]
	e1 := b - a
	e2 := c - a
	n := e1.cross(e2)
	assert n.x * a.x + n.y * a.y + n.z * a.z > 0
}

fn test_cylinder_triangles_build_rings_and_caps() {
	verts := cylinder_triangles(Vec3{0, 0, 0}, Vec3{0, 3, 0}, 1.0, 8)
	assert verts.len == 8 * 12
	for v in verts {
		r := math.sqrt(v.x * v.x + v.z * v.z)
		assert r <= 1.0 + 1e-4
		assert v.y >= -1e-4 && v.y <= 3.0 + 1e-4
	}
}

fn test_plane_and_grid_helpers_accept_valid_arguments() {
	verts := sphere_triangles(Vec3{}, 1.0, 0, 8)
	assert verts.len == 0
	bad_cylinder := cylinder_triangles(Vec3{}, Vec3{}, 1.0, 8)
	assert bad_cylinder.len == 0
}
