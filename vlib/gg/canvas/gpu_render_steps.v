module canvas

import encoding.binary
import math

// Static and per-instance data of the Graphite render steps. The layouts and
// constants match skgpu::graphite::AnalyticRRectRenderStep and
// CoverBoundsRenderStep, whose shaders are embedded unchanged.

const analytic_rrect_vertex_stride = 28
const analytic_rrect_instance_stride = 108
const analytic_rrect_index_count = 69
const cover_bounds_instance_stride = 60

const corner_vertex_count = 9
// Instance values of the interior fill behavior and of the AA radius signal.
const solid_interior = f32(1)
const stroke_interior = f32(0)
const filled_stroke_interior = f32(-1)
const complex_aa_insets = f32(-1)

// kHR2 = 0.5f * SK_FloatSqrt2
const half_root_2 = f32(0.5) * f32(1.41421356)

const corner_tl = u16(0)
const corner_tr = u16(corner_vertex_count)
const corner_br = u16(2 * corner_vertex_count)
const corner_bl = u16(3 * corner_vertex_count)

const analytic_rrect_indices = [
	// Exterior AA ramp outset
	corner_tl + 0,
	corner_tl + 4,
	corner_tl + 1,
	corner_tl + 5,
	corner_tl + 2,
	corner_tl + 3,
	corner_tl + 5,
	corner_tr + 0,
	corner_tr + 4,
	corner_tr + 1,
	corner_tr + 5,
	corner_tr + 2,
	corner_tr + 3,
	corner_tr + 5,
	corner_br + 0,
	corner_br + 4,
	corner_br + 1,
	corner_br + 5,
	corner_br + 2,
	corner_br + 3,
	corner_br + 5,
	corner_bl + 0,
	corner_bl + 4,
	corner_bl + 1,
	corner_bl + 5,
	corner_bl + 2,
	corner_bl + 3,
	corner_bl + 5,
	corner_tl + 0,
	corner_tl + 4,
	// Outer to inner edges
	corner_tl + 4,
	corner_tl + 6,
	corner_tl + 5,
	corner_tl + 7,
	corner_tr + 4,
	corner_tr + 6,
	corner_tr + 5,
	corner_tr + 7,
	corner_br + 4,
	corner_br + 6,
	corner_br + 5,
	corner_br + 7,
	corner_bl + 4,
	corner_bl + 6,
	corner_bl + 5,
	corner_bl + 7,
	corner_tl + 4,
	corner_tl + 6,
	// Fill triangles
	corner_tl + 6,
	corner_tl + 8,
	corner_tl + 7,
	corner_tl + 7,
	corner_tr + 8,
	corner_tr + 6,
	corner_tr + 8,
	corner_tr + 7,
	corner_tr + 7,
	corner_br + 8,
	corner_br + 6,
	corner_br + 8,
	corner_br + 7,
	corner_br + 7,
	corner_bl + 8,
	corner_bl + 6,
	corner_bl + 8,
	corner_bl + 7,
	corner_bl + 7,
	corner_tl + 8,
	corner_tl + 6,
]!

// analytic_rrect_vertex_data returns the 36 template vertices: 9 per corner,
// each `{uint cornerID, float2 position, float2 normal, float normalScale,
// float centerWeight}`.
fn analytic_rrect_vertex_data() []u8 {
	outset := f32(1)
	inset := f32(-1)
	center := f32(1)
	mut out := []u8{cap: 4 * corner_vertex_count * analytic_rrect_vertex_stride}
	for corner in u32(0) .. 4 {
		template := [
			// Device-space AA outsets from outer curve
			[f32(1), 0, 1, 0, outset, 0],
			[f32(1), 0, half_root_2, half_root_2, outset, 0],
			[f32(0), 1, half_root_2, half_root_2, outset, 0],
			[f32(0), 1, 0, 1, outset, 0],
			// Outer anchors
			[f32(1), 0, half_root_2, half_root_2, 0, 0],
			[f32(0), 1, half_root_2, half_root_2, 0, 0],
			// Inner curve
			[f32(1), 0, 1, 0, inset, 0],
			[f32(0), 1, 0, 1, inset, 0],
			// Center filling vertex
			[f32(1), 0, 1, 0, inset, center],
		]
		for values in template {
			put_u32(mut out, corner)
			for v in values {
				put_f32(mut out, v)
			}
		}
	}
	return out
}

@[inline]
fn put_f32(mut out []u8, v f32) {
	mut bytes := [4]u8{}
	binary.little_endian_put_u32_fixed(mut bytes, math.f32_bits(v))
	out << bytes[..]
}

@[inline]
fn put_u32(mut out []u8, v u32) {
	mut bytes := [4]u8{}
	binary.little_endian_put_u32_fixed(mut bytes, v)
	out << bytes[..]
}

fn put_matrix_columns(mut out []u8, m Matrix) {
	// mat0, mat1, mat2 are the x, y and translation columns of the 3x3 matrix,
	// each with its perspective row: m.rc(0,0), m.rc(1,0), m.rc(3,0), ...
	v := m.m
	for column in [0, 1, 2] {
		put_f32(mut out, v[column])
		put_f32(mut out, v[3 + column])
		put_f32(mut out, v[6 + column])
	}
}

// depth_as_float maps a painter's order depth to the depth attachment value,
// like DrawOrder::depthAsFloat().
fn depth_as_float(depth u16) f32 {
	return f32(1) - f32(depth) / f32(0xffff)
}

// GeometryKind distinguishes the shapes the analytic render step encodes.
enum GeometryKind {
	rect
	rrect
}

struct ShapeGeometry {
	kind  GeometryKind
	rect  Rect
	rrect RRect
}

fn (g ShapeGeometry) bounds() Rect {
	return if g.kind == .rect { g.rect } else { g.rrect.rect }
}

fn (g ShapeGeometry) is_rect() bool {
	return g.kind == .rect || g.rrect.is_rect()
}

// write_analytic_rrect_instance mirrors AnalyticRRectRenderStep::writeVertices()
// for filled and stroked rects and rrects.
fn write_analytic_rrect_instance(mut out []u8, geometry ShapeGeometry, transform Transform, paint Paint, depth u16, ssbo_index u32) {
	bounds := geometry.bounds()
	mut aa_radius := transform.local_aa_radius()
	mut stroke_inset := f32(0)
	mut center_weight := solid_interior
	stroked := paint.style == .stroke
	if stroked {
		stroke_radius := max_f32(0, paint.stroke_radius())
		size_x := bounds.width()
		size_y := bounds.height()
		// skvx: size - 2 * halfWidth, the product rounded first.
		double_radius := f32(2) * stroke_radius
		gap_x := size_x - double_radius
		gap_y := size_y - double_radius
		if (gap_x <= 0 || gap_y <= 0) && stroke_radius > 0 {
			stroke_inset = -stroke_radius
		} else {
			center_weight = stroke_interior
			stroke_inset = stroke_radius
		}
		mut x_radii := [4]f32{}
		if geometry.kind == .rrect {
			for i in 0 .. 4 {
				x_radii[i] = geometry.rrect.radii[i].x
			}
		}
		if stroke_radius > 0 {
			mut join_style := stroke_join_limit(paint)
			empty_x := size_x == 0
			empty_y := size_y == 0
			if empty_x && empty_y {
				join_style = match paint.cap {
					.round { -1 }
					.butt { 0 }
					.square { 1 }
				}
			} else if stroke_join_limit(paint) > 0 {
				if paint.miter_limit < f32(math.sqrt2) || empty_x || empty_y {
					join_style = 0
				} else {
					join_style = 1
				}
			}
			put_f32(mut out, -2)
			put_f32(mut out, 0)
			put_f32(mut out, stroke_radius)
			put_f32(mut out, join_style)
			for v in x_radii {
				put_f32(mut out, v)
			}
			put_ltrb(mut out, bounds)
		} else {
			for v in x_radii {
				put_f32(mut out, f32(-2) - v)
			}
			for i in 0 .. 4 {
				put_f32(mut out, if geometry.kind == .rrect {
					geometry.rrect.radii[i].y
				} else {
					f32(0)
				})
			}
			put_ltrb(mut out, bounds)
		}
	} else if geometry.is_rect() {
		// Rectangles become per-edge AA quads with all edges anti-aliased.
		for _ in 0 .. 4 {
			put_f32(mut out, -1)
		}
		for v in [bounds.left(), bounds.right(), bounds.right(), bounds.left()] {
			put_f32(mut out, v)
		}
		for v in [bounds.top(), bounds.top(), bounds.bottom(), bounds.bottom()] {
			put_f32(mut out, v)
		}
	} else {
		for i in 0 .. 4 {
			put_f32(mut out, geometry.rrect.radii[i].x)
		}
		for i in 0 .. 4 {
			put_f32(mut out, geometry.rrect.radii[i].y)
		}
		put_ltrb(mut out, bounds)
	}
	if opposite_insets_intersect(geometry, stroke_inset, aa_radius) {
		aa_radius = complex_aa_insets
		if center_weight == stroke_interior {
			center_weight = filled_stroke_interior
		}
	}
	c := bounds.center()
	put_f32(mut out, c.x)
	put_f32(mut out, c.y)
	put_f32(mut out, center_weight)
	put_f32(mut out, aa_radius)
	put_f32(mut out, depth_as_float(depth))
	put_u32(mut out, ssbo_index)
	put_matrix_columns(mut out, transform.matrix)
}

// stroke_join_limit mirrors StrokeStyle::joinLimit(): -1 for round joins, 0
// for bevel joins and the miter limit otherwise.
fn stroke_join_limit(paint Paint) f32 {
	return match paint.join {
		.round { -1 }
		.bevel { 0 }
		.miter { max_f32(0, paint.miter_limit) }
	}
}

fn put_ltrb(mut out []u8, r Rect) {
	put_f32(mut out, r.left())
	put_f32(mut out, r.top())
	put_f32(mut out, r.right())
	put_f32(mut out, r.bottom())
}

fn opposite_insets_intersect(geometry ShapeGeometry, stroke_radius f32, aa_radius f32) bool {
	if geometry.kind == .rect {
		limit := f32(2) * (stroke_radius + aa_radius)
		return geometry.rect.width() <= limit || geometry.rect.height() <= limit
	}
	rr := geometry.rrect
	double_aa := f32(2) * aa_radius
	max_inset := stroke_radius + double_aa
	w := rr.rect.width()
	h := rr.rect.height()
	return max_inset >= w - rr.radii[3].x || max_inset >= w - rr.radii[2].x
		|| max_inset >= w - rr.radii[0].x || max_inset >= w - rr.radii[1].x
		|| max_inset >= h - rr.radii[3].y || max_inset >= h - rr.radii[2].y
		|| max_inset >= h - rr.radii[0].y || max_inset >= h - rr.radii[1].y
}

// write_cover_bounds_instance mirrors CoverBoundsRenderStep::writeVertices()
// for regular (not inverse) fills.
fn write_cover_bounds_instance(mut out []u8, bounds Rect, transform Transform, depth u16, ssbo_index u32) {
	put_ltrb(mut out, bounds)
	put_f32(mut out, depth_as_float(depth))
	put_u32(mut out, ssbo_index)
	put_matrix_columns(mut out, transform.matrix)
}

// The convex TessellateWedgesRenderStep instance with infinity support:
// float4 p01, float4 p23, float2 fanPointAttrib, float depth, uint ssboIndex.
const wedge_instance_stride = 48
const wedge_ssbo_offset = 44

// The static data of a wedge draw at resolve level 0, the only level lines
// need: FixedCountWedges' fan vertex and the curve's end vertices
// (resolveLevel, idx), and its fan triangle.
const wedge_vertices = [f32(-1), -1, 0, 0, 0, 1]!
const wedge_indices = [u16(0), 1, 2]!
const wedge_index_count = 3

// write_rect_wedge_patches mirrors TessellateWedgesRenderStep::writeVertices()
// for a device-space rectangle: the four lines of SkPath::Rect() around the
// contour's midpoint (MidpointContourParser), each written by
// PatchWriter::writeLine() as a cubic patch.
fn write_rect_wedge_patches(mut out []u8, r Rect, depth u16) {
	pts := [Point{r.left(), r.top()}, Point{r.right(), r.top()}, Point{r.right(), r.bottom()},
		Point{r.left(), r.bottom()}]!
	// The lines' end points, then the start point of the implicit close.
	mut mx := pts[1].x + pts[2].x
	mx = mx + pts[3].x
	mx = mx + pts[0].x
	mut my := pts[1].y + pts[2].y
	my = my + pts[3].y
	my = my + pts[0].y
	fan := Point{mx * f32(0.25), my * f32(0.25)}
	for i in 0 .. 4 {
		write_line_patch(mut out, pts[i], pts[(i + 1) % 4], fan, depth)
	}
}

// write_line_patch mirrors PatchWriter::writeLine() without replicated end
// points: the cubic (p0, (p1 - p0) / 3 + p0, (p0 - p1) / 3 + p1, p1), with
// separate operations because Skia does not contract them into FMAs.
fn write_line_patch(mut out []u8, p0 Point, p1 Point, fan Point, depth u16) {
	third := f32(1) / f32(3)
	d1x := (p1.x - p0.x) * third
	d1y := (p1.y - p0.y) * third
	d2x := (p0.x - p1.x) * third
	d2y := (p0.y - p1.y) * third
	c1 := Point{d1x + p0.x, d1y + p0.y}
	c2 := Point{d2x + p1.x, d2y + p1.y}
	for v in [p0.x, p0.y, c1.x, c1.y, c2.x, c2.y, p1.x, p1.y, fan.x, fan.y]! {
		put_f32(mut out, v)
	}
	put_f32(mut out, depth_as_float(depth))
	put_u32(mut out, 0)
}
