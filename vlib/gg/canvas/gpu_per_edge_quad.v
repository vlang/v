module canvas

// PerEdgeAAQuadRenderStep's static template and 80-byte instance layout.
const per_edge_quad_vertex_stride = 12
const per_edge_quad_instance_stride = 80
const per_edge_quad_index_count = 29

const per_edge_quad_indices = [u16(1), 2, 3, 4, 7, 5, 5, 6, 7, 8, 11, 9, 9, 10, 11, 12, 15, 13,
	13, 14, 15, 0, 3, 1, 3, 3, 7, 15, 11]!

fn per_edge_quad_vertex_data() []u8 {
	mut out := []u8{cap: 16 * per_edge_quad_vertex_stride}
	for corner in u32(0) .. 4 {
		for normal in [Point{1, 0}, Point{half_root_2, half_root_2}, Point{0, 1}, Point{}] {
			put_u32(mut out, corner)
			put_f32(mut out, normal.x)
			put_f32(mut out, normal.y)
		}
	}
	return out
}

// Layer restores use a clockwise rectangle with AA on all four edges.
// This mirrors PerEdgeAAQuadRenderStep::writeVertices() for that geometry.
fn write_per_edge_quad_instance(mut out []u8, rect Rect, transform Transform, depth u16, ssbo u32) {
	out << [u8(255), 255, 255, 255]
	for x in [rect.left(), rect.right(), rect.right(), rect.left()] {
		put_f32(mut out, x)
	}
	for y in [rect.top(), rect.top(), rect.bottom(), rect.bottom()] {
		put_f32(mut out, y)
	}
	put_f32(mut out, depth_as_float(depth))
	put_u32(mut out, ssbo)
	put_matrix_columns(mut out, transform.matrix)
}
