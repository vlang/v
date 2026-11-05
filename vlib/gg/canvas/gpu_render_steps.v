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

// FixedCountWedges' template: kMaxResolveLevel and the index count of a
// draw at resolve level 0 (only the fan triangle).
const max_resolve_level = 5
const wedge_base_index_count = u32(3)
// kMaxParametricSegments^4: curves needing more segments are chopped.
const max_parametric_segments_p4 = f32(32 * 32 * 32 * 32)

// wedge_vertex_data mirrors FixedCountWedges::WriteVertexBuffer(): the fan
// vertex, then (resolveLevel, idx) for the curve vertices of every level.
fn wedge_vertex_data() []f32 {
	mut v := [f32(-1), -1, 0, 0, 0, 1]
	for level in 1 .. max_resolve_level + 1 {
		for i := 1; i < 1 << level; i += 2 {
			v << f32(level)
			v << f32(i)
		}
	}
	return v
}

// wedge_index_data mirrors FixedCountWedges::WriteIndexBuffer(): the fan
// triangle, then the curve triangles of every level, starting at vertex 1.
fn wedge_index_data() []u16 {
	mut out := [u16(0), 1, 2]
	base := u16(1)
	mut tris := [[base, base + 2, base + 1]!]
	mut neighbor := 0
	mut next := base + 3
	for level in 2 .. max_resolve_level + 1 {
		pairs := (1 << (level - 1)) >> 1
		for _ in 0 .. pairs {
			n := tris[neighbor]
			tris << [n[0], next, n[1]]!
			next++
			tris << [n[1], next, n[2]]!
			next++
			neighbor++
		}
	}
	for t in tris {
		out << t[0]
		out << t[1]
		out << t[2]
	}
	return out
}

// wedge_vertex_count mirrors FixedCountWedges::VertexCount(): the index
// count of a draw whose patches need n^4 = `n4` parametric segments.
fn wedge_vertex_count(n4 f32) u32 {
	mut level := nextlog16(n4)
	if level > max_resolve_level {
		level = max_resolve_level
	}
	return u32(((1 << level) - 1 + 1) * 3)
}

// nextlog2 mirrors wangs_formula::nextlog2(): ceil(log2(x)) for x > 1.
fn nextlog2(x f32) int {
	if x <= 1 {
		return 0
	}
	bits := math.f32_bits(x) + (u32(1) << 23) - 1
	exp := int((bits >> 23) & 0xff) - 127
	return if exp > 0 { exp } else { 0 }
}

fn nextlog16(x f32) int {
	return (nextlog2(x) + 3) >> 2
}

// sk_root2_over_2 is SK_ScalarRoot2Over2, the weight of quarter-circle conics.
const sk_root2_over_2 = f32(0.707106781)

// element_path mirrors Shape::asPath() for rects and rrects: SkPathBuilder's
// addRect() and addRRect() with their default (clockwise) directions and
// start indices, through SkPathRawShapes.
fn element_path(shape ClipShape) Path {
	if shape.kind == .rect {
		return rect_path(shape.rect, 0)
	}
	rr := shape.rrect
	if rr.is_rect() || rr.is_empty() {
		return rect_path(rr.rect, (6 + 1) / 2)
	}
	if rr.typ == .oval {
		return oval_path(rr.rect, 6 / 2)
	}
	return rrect_path(rr, 6)
}

fn rect_points(r Rect) [4]Point {
	return [Point{r.left(), r.top()}, Point{r.right(), r.top()}, Point{r.right(), r.bottom()},
		Point{r.left(), r.bottom()}]!
}

fn rect_path(r Rect, start int) Path {
	return rect_path_dir(r, .cw, start)
}

fn oval_path(r Rect, start int) Path {
	return oval_path_dir(r, .cw, start)
}

// rrect_path mirrors set_as_rrect() for a clockwise contour.
fn rrect_path(rr RRect, start int) Path {
	return rrect_path_dir(rr, .cw, start)
}

// Contour is a contour found by MidpointContourParser: its verbs, the index
// of its first point and conic weight, and its fan point.
struct Contour {
	verb_start   int
	verb_end     int
	point_start  int
	weight_start int
	midpoint     Point
}

// midpoint_contours mirrors MidpointContourParser: contours start at their
// last leading move, and the midpoint averages the verbs' end points and the
// start point unless the contour already ends there.
fn midpoint_contours(path Path) []Contour {
	mut out := []Contour{}
	n := path.verbs.len
	mut base_v, mut base_p, mut base_w := 0, 0, 0
	mut vidx, mut pidx, mut widx := 0, 0, 0
	for {
		mut has_geometry := false
		mut mid := Point{0, 0}
		mut weight := 0
		mut found := false
		for ; base_v + vidx < n; vidx++ {
			match path.verbs[base_v + vidx] {
				.move {
					if !has_geometry {
						mid = Point{0, 0}
						weight = 0
						base_v += vidx
						vidx = 0
						base_p += pidx
						base_w += widx
						widx = 0
						pidx = 1
						continue
					}
					found = true
					break
				}
				.line {
					pidx++
				}
				.conic {
					widx++
					pidx += 2
				}
				.quad {
					pidx += 2
				}
				.cubic {
					pidx += 3
				}
				.close {
					continue
				}
			}
			p := path.points[base_p + pidx - 1]
			mid = Point{mid.x + p.x, mid.y + p.y}
			weight++
			has_geometry = true
		}
		if !has_geometry {
			break
		}
		first := path.points[base_p]
		last := path.points[base_p + pidx - 1]
		if first.x != last.x || first.y != last.y {
			mid = Point{mid.x + first.x, mid.y + first.y}
			weight++
		}
		scale := f32(1) / f32(weight)
		out << Contour{
			verb_start:   base_v
			verb_end:     base_v + vidx
			point_start:  base_p
			weight_start: base_w
			midpoint:     Point{mid.x * scale, mid.y * scale}
		}
		if !found {
			break
		}
	}
	return out
}

// write_wedge_patches mirrors TessellateWedgesRenderStep::writeVertices() for
// a path drawn with `m`. It returns the number of patches and the largest n^4
// they need; curves that would have to be chopped are not supported.
fn write_wedge_patches(mut out []u8, path Path, m Matrix, depth u16) !(int, f32) {
	mut count := 0
	mut max_n4 := f32(1)
	for contour in midpoint_contours(path) {
		fan := contour.midpoint
		mut pi := contour.point_start
		mut wi := contour.weight_start
		mut start := Point{0, 0}
		mut last := Point{0, 0}
		for vi in contour.verb_start .. contour.verb_end {
			mut n4 := f32(1)
			mut written := false
			match path.verbs[vi] {
				.move {
					start = path.points[pi]
					last = start
					pi++
				}
				.line {
					p1 := path.points[pi]
					written = write_line_patch(mut out, last, p1, fan, depth)
					last = p1
					pi++
				}
				.quad {
					p1 := path.points[pi]
					p2 := path.points[pi + 1]
					n4 = quadratic_p4(f32(4), last, p1, p2, m)
					if n4 > max_parametric_segments_p4 {
						return error('gg.canvas: curves that need chopping are not supported')
					}
					// writeQuadPatch(): the equivalent cubic.
					two_thirds := f32(2) / f32(3)
					c1 := mix_point(last, p1, two_thirds)
					c2 := mix_point(p2, p1, two_thirds)
					written = write_cubic_patch(mut out, last, c1, c2, p2, fan, depth)
					last = p2
					pi += 2
				}
				.conic {
					p1 := path.points[pi]
					p2 := path.points[pi + 1]
					w := path.weights[wi]
					n2 := conic_p2(f32(4), last, p1, p2, w, m)
					n4 = n2 * n2
					if n4 > max_parametric_segments_p4 {
						return error('gg.canvas: curves that need chopping are not supported')
					}
					written = write_conic_patch(mut out, last, p1, p2, w, fan, depth)
					last = p2
					pi += 2
					wi++
				}
				.cubic {
					p1 := path.points[pi]
					p2 := path.points[pi + 1]
					p3 := path.points[pi + 2]
					n4 = cubic_p4(f32(4), last, p1, p2, p3, m)
					if n4 > max_parametric_segments_p4 {
						return error('gg.canvas: curves that need chopping are not supported')
					}
					written = write_cubic_patch(mut out, last, p1, p2, p3, fan, depth)
					last = p3
					pi += 3
				}
				.close {}
			}
			if written {
				count++
				// The draw's template covers its patches' largest tolerance
				// (lines need one segment, curves at least one).
				if n4 > max_n4 {
					max_n4 = n4
				}
			}
		}
		if last.x != start.x || last.y != start.y {
			if write_line_patch(mut out, last, start, fan, depth) {
				count++
			}
		}
	}
	return count, max_n4
}

// mix_point is PatchWriter's mix(): (b - a) * t + a, one operation at a time.
fn mix_point(a Point, b Point, t f32) Point {
	dx := b.x - a.x
	dy := b.y - a.y
	sx := dx * t
	sy := dy * t
	return Point{sx + a.x, sy + a.y}
}

// quadratic_p4 mirrors wangs_formula::quadratic_p4(); length_term_p2<2>(4)
// is 1.
fn quadratic_p4(precision f32, p0 Point, p1 Point, p2 Point, m Matrix) f32 {
	kx := f32(-2) * p1.x
	ky := f32(-2) * p1.y
	ax := kx + p0.x
	ay := ky + p0.y
	v := xform_vector(m, Point{ax + p2.x, ay + p2.y})
	vv := dot2(v, v)
	term := (f32(4) / f32(64)) * (precision * precision)
	return vv * term
}

// cubic_p4 mirrors wangs_formula::cubic_p4(); length_term_p2<3>(4) is 9.
fn cubic_p4(precision f32, p0 Point, p1 Point, p2 Point, p3 Point, m Matrix) f32 {
	k1x := f32(-2) * p1.x
	k1y := f32(-2) * p1.y
	k2x := f32(-2) * p2.x
	k2y := f32(-2) * p2.y
	a1x := k1x + p0.x
	a1y := k1y + p0.y
	a2x := k2x + p1.x
	a2y := k2y + p1.y
	v1 := xform_vector(m, Point{a1x + p2.x, a1y + p2.y})
	v2 := xform_vector(m, Point{a2x + p3.x, a2y + p3.y})
	term := (f32(9 * 4) / f32(64)) * (precision * precision)
	return max_f32(dot2(v1, v1), dot2(v2, v2)) * term
}

// write_cubic_patch mirrors PatchWriter::writeCubicPatch(); degenerate
// patches are skipped.
fn write_cubic_patch(mut out []u8, p0 Point, p1 Point, p2 Point, p3 Point, fan Point, depth u16) bool {
	if p0.x == p1.x && p0.y == p1.y && p1.x == p2.x && p1.y == p2.y && p2.x == p3.x && p2.y == p3.y {
		return false
	}
	for v in [p0.x, p0.y, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, fan.x, fan.y]! {
		put_f32(mut out, v)
	}
	put_f32(mut out, depth_as_float(depth))
	put_u32(mut out, 0)
	return true
}

// conic_p2 mirrors wangs_formula::conic_p2() with the 2x2 of `m` as the
// vector transform, one operation per statement (-ffp-contract=off).
fn conic_p2(precision f32, q0 Point, q1 Point, q2 Point, w f32, m Matrix) f32 {
	mut p0 := xform_vector(m, q0)
	mut p1 := xform_vector(m, q1)
	mut p2 := xform_vector(m, q2)
	min_x := min_f32(min_f32(p0.x, p1.x), p2.x)
	min_y := min_f32(min_f32(p0.y, p1.y), p2.y)
	max_x := max_f32(max_f32(p0.x, p1.x), p2.x)
	max_y := max_f32(max_f32(p0.y, p1.y), p2.y)
	sum_x := min_x + max_x
	sum_y := min_y + max_y
	cx := f32(0.5) * sum_x
	cy := f32(0.5) * sum_y
	p0 = Point{p0.x - cx, p0.y - cy}
	p1 = Point{p1.x - cx, p1.y - cy}
	p2 = Point{p2.x - cx, p2.y - cy}
	max_len := C.sqrtf(max_f32(dot2(p0, p0), max_f32(dot2(p1, p1), dot2(p2, p2))))
	k := f32(-2) * w
	kx := k * p1.x
	ky := k * p1.y
	ax := kx + p0.x
	ay := ky + p0.y
	dp := Point{ax + p2.x, ay + p2.y}
	dw := abs_f32(k + f32(2))
	len_precision := max_len * precision
	rp_minus_1 := max_f32(0, len_precision - f32(1))
	dp_len := C.sqrtf(dot2(dp, dp))
	a := dp_len * precision
	b := rp_minus_1 * dw
	numer := a + b
	denom := f32(4) * min_f32(w, 1)
	return numer / denom
}

// xform_vector applies the 2x2 of `m` (VectorXform: fC0 * x + fC1 * y).
fn xform_vector(m Matrix, v Point) Point {
	ax := m.m[0] * v.x
	ay := m.m[3] * v.x
	bx := m.m[1] * v.y
	by := m.m[4] * v.y
	return Point{ax + bx, ay + by}
}

fn dot2(a Point, b Point) f32 {
	x := a.x * b.x
	y := a.y * b.y
	return x + y
}

// write_conic_patch mirrors PatchWriter::writeConicPatch(): the control
// points and (w, inf) mark a conic. Degenerate patches are skipped.
fn write_conic_patch(mut out []u8, p0 Point, p1 Point, p2 Point, w f32, fan Point, depth u16) bool {
	if p0.x == p1.x && p0.y == p1.y && p1.x == p2.x && p1.y == p2.y {
		return false
	}
	inf := f32(math.inf(1))
	for v in [p0.x, p0.y, p1.x, p1.y, p2.x, p2.y, w, inf, fan.x, fan.y]! {
		put_f32(mut out, v)
	}
	put_f32(mut out, depth_as_float(depth))
	put_u32(mut out, 0)
	return true
}

// write_rect_wedge_patches mirrors TessellateWedgesRenderStep::writeVertices()
// for a device-space rectangle: the four lines of SkPath::Rect() around the
// contour's midpoint (MidpointContourParser), each written by
// PatchWriter::writeLine() as a cubic patch.
fn write_rect_wedge_patches(mut out []u8, r Rect, depth u16) {
	write_wedge_patches(mut out, rect_path(r, 0), Matrix.identity(), depth) or { panic(err) }
}

// write_line_patch mirrors PatchWriter::writeLine() without replicated end
// points: the cubic (p0, (p1 - p0) / 3 + p0, (p0 - p1) / 3 + p1, p1), with
// separate operations because Skia does not contract them into FMAs.
fn write_line_patch(mut out []u8, p0 Point, p1 Point, fan Point, depth u16) bool {
	if p0.x == p1.x && p0.y == p1.y {
		// Degenerate patches are skipped.
		return false
	}
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
	return true
}
