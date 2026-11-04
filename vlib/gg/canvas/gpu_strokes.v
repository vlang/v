module canvas

import math

// Graphite's TessellateStrokesRenderStep: strokes of paths, and of rrects
// that the analytic renderer cannot stroke, drawn as patches of a fixed-count
// triangle strip in a 4x MSAA pass.

// The stroke instance with infinity support: float4 p01, float4 p23, float2
// prevPoint (the join control point), float2 stroke (radius, join type),
// float depth, uint ssboIndex.
const stroke_instance_stride = 56
const stroke_ssbo_offset = 52
// The stroke uniforms: float4 color, float4 affineMatrix, float2 translate
// and float maxScale, padded to 48 bytes.
const stroke_uniform_size = 48
// FixedCountStrokes::kMaxEdges
const max_stroke_edges = (1 << 14) - 1

const cubic_curve_type = f32(0)
const conic_curve_type = f32(1)

// StrokeTolerances mirrors LinearTolerances for strokes.
struct StrokeTolerances {
mut:
	n4                f32 = 1
	radial_per_radian f32
	edges_in_joins    int
}

fn (mut t StrokeTolerances) accumulate(o StrokeTolerances) {
	if o.n4 > t.n4 {
		t.n4 = o.n4
	}
	if o.radial_per_radian > t.radial_per_radian {
		t.radial_per_radian = o.radial_per_radian
	}
	if o.edges_in_joins > t.edges_in_joins {
		t.edges_in_joins = o.edges_in_joins
	}
}

// required_stroke_edges mirrors LinearTolerances::requiredStrokeEdges().
fn (t StrokeTolerances) required_stroke_edges() int {
	radial := t.radial_per_radian * sk_scalar_pi
	max_radial := math.max(int(C.ceilf(radial)), 1)
	max_parametric := int(C.ceilf(C.sqrtf(C.sqrtf(t.n4))))
	return t.edges_in_joins + max_radial + max_parametric
}

// stroke_vertex_count mirrors FixedCountStrokes::VertexCount().
fn stroke_vertex_count(t StrokeTolerances) u32 {
	return u32(math.min(t.required_stroke_edges(), max_stroke_edges) * 2)
}

fn C.acosf(x f32) f32

// set_stroke mirrors LinearTolerances::setStroke().
fn (mut t StrokeTolerances) set_stroke(radius f32, join_type f32, max_scale f32) {
	approx_radius := if radius == 0 { f32(0.5) } else { radius * max_scale }
	quarter := f32(1) / f32(4)
	cos_theta := f32(1) - quarter / approx_radius
	t.radial_per_radian = f32(0.5) / C.acosf(max_f32(cos_theta, -1))
	t.edges_in_joins = if join_type > 0 { 4 } else { 3 }
	if join_type < 0 && t.radial_per_radian > 0 {
		t.edges_in_joins += int(C.ceilf(t.radial_per_radian * sk_scalar_pi)) - 1
	}
}

// StrokeWriter mirrors PatchWriter with ReplicateLineEndPoints and
// TrackJoinControlPoints, writing TessellateStrokesRenderStep's patches.
struct StrokeWriter {
	m         Matrix
	radius    f32
	join_type f32
	depth     f32
mut:
	out        []u8
	count      int
	tolerances StrokeTolerances
	// The accumulated tolerances of the written patches (the draw's template).
	template StrokeTolerances
	join     Point
	// The deferred first patch of the contour.
	first_point Point
	last_point  Point
	curve_type  f32 = -1
	deferred_n4 f32 = -1
	deferred    []u8
}

fn new_stroke_writer(m Matrix, max_scale f32, radius f32, join_type f32, depth f32) StrokeWriter {
	mut w := StrokeWriter{
		m:         m
		radius:    radius
		join_type: join_type
		depth:     depth
	}
	w.tolerances.set_stroke(radius, join_type, max_scale)
	return w
}

fn (w &StrokeWriter) must_defer() bool {
	return w.deferred_n4 < 0
}

fn (w &StrokeWriter) has_pending() bool {
	return w.deferred_n4 > 0
}

// append mirrors DynamicInstancesPatchAllocator::append().
fn (mut w StrokeWriter) append(data []u8) {
	w.out << data
	w.count++
	w.template.accumulate(w.tolerances)
}

fn (w &StrokeWriter) patch_bytes(p0 Point, p1 Point, p2 Point, p3 Point, join Point) []u8 {
	mut b := []u8{cap: stroke_instance_stride}
	for v in [p0.x, p0.y, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, join.x, join.y, w.radius, w.join_type,
		w.depth]! {
		put_f32(mut b, v)
	}
	put_u32(mut b, 0)
	return b
}

// write_patch mirrors PatchWriter::writePatch().
fn (mut w StrokeWriter) write_patch(p0 Point, p1 Point, p2 Point, p3 Point, curve_type f32) {
	w.curve_type = curve_type
	last := if curve_type == cubic_curve_type { p3 } else { p2 }
	if p0 == p1 && p1 == p2 && p2 == last {
		return
	}
	data := w.patch_bytes(p0, p1, p2, p3, w.join)
	if w.must_defer() {
		w.deferred_n4 = w.tolerances.n4
		w.deferred = data
	} else {
		w.append(data)
	}
	w.last_point = last
	w.join = tangent_point(last, p2, p1, p0)
}

fn tangent_point(p0 Point, p1 Point, p2 Point, p3 Point) Point {
	if p0 != p1 {
		return p1
	} else if p2 != p1 {
		return p2
	}
	return p3
}

// account_for_curve mirrors PatchWriter::accountForCurve(); chopping curves
// with more parametric segments is not ported.
fn (mut w StrokeWriter) account_for_curve(n4 f32) ! {
	if n4 > max_parametric_segments_p4 {
		return error('gg.canvas: stroked curves that need chopping are not supported')
	}
	w.tolerances.n4 = max_f32(1, n4)
}

fn (mut w StrokeWriter) write_line(p0 Point, p1 Point) {
	w.tolerances.n4 = 1
	w.write_patch(p0, p0, p1, p1, cubic_curve_type)
}

fn (mut w StrokeWriter) write_quadratic(p0 Point, p1 Point, p2 Point) ! {
	w.account_for_curve(quadratic_p4(f32(4), p0, p1, p2, w.m))!
	two_thirds := f32(2) / f32(3)
	w.write_patch(p0, mix_point(p0, p1, two_thirds), mix_point(p2, p1, two_thirds), p2,
		cubic_curve_type)
}

fn (mut w StrokeWriter) write_conic(p0 Point, p1 Point, p2 Point, weight f32) ! {
	n2 := conic_p2(f32(4), p0, p1, p2, weight, w.m)
	w.account_for_curve(n2 * n2)!
	w.write_patch(p0, p1, p2, Point{weight, f32(math.inf(1))}, conic_curve_type)
}

fn (mut w StrokeWriter) write_cubic(p0 Point, p1 Point, p2 Point, p3 Point) ! {
	w.account_for_curve(cubic_p4(f32(4), p0, p1, p2, p3, w.m))!
	w.write_patch(p0, p1, p2, p3, cubic_curve_type)
}

// write_circle mirrors PatchWriter::writeCircle(): a round cap or cusp.
fn (mut w StrokeWriter) write_circle(p Point) {
	w.tolerances.n4 = 1
	w.append(w.patch_bytes(p, p, p, p, p))
}

// write_square mirrors PatchWriter::writeSquare(): a square cap.
fn (mut w StrokeWriter) write_square(p Point, join_to ?Point) {
	w.tolerances.n4 = 1
	w.append(w.patch_bytes(p, p, p, Point{1, f32(math.inf(1))}, join_to or { p }))
}

// write_deferred_stroke_patch mirrors PatchWriter::writeDeferredStrokePatch().
fn (mut w StrokeWriter) write_deferred_stroke_patch(move_to Point, cap ?StrokeCap) {
	if w.has_pending() {
		mut join := w.join
		if c := cap {
			join = w.first_point
			if c == .round {
				w.write_circle(w.last_point)
				w.write_circle(w.first_point)
			} else if c == .square {
				w.write_square(w.last_point, w.join)
				f := unsafe { &f32(w.deferred.data) }
				p0 := Point{unsafe { f[0] }, unsafe { f[1] }}
				p1 := Point{unsafe { f[2] }, unsafe { f[3] }}
				p2 := Point{unsafe { f[4] }, unsafe { f[5] }}
				last := if w.curve_type == cubic_curve_type {
					Point{unsafe { f[6] }, unsafe { f[7] }}
				} else {
					p2
				}
				w.write_square(p0, tangent_point(p0, p1, p2, last))
			}
		}
		mut patch := w.deferred.clone()
		mut j := []u8{}
		put_f32(mut j, join.x)
		put_f32(mut j, join.y)
		for i in 0 .. 8 {
			patch[32 + i] = j[i]
		}
		w.tolerances.n4 = w.deferred_n4
		w.append(patch)
	} else if c := cap {
		if w.curve_type >= 0 {
			match c {
				.butt {}
				.round { w.write_circle(w.first_point) }
				.square { w.write_square(w.first_point, none) }
			}
		}
	}
	w.curve_type = -1
	w.deferred_n4 = -1
	w.first_point = move_to
	w.last_point = move_to
}

// close_deferred_stroke_patch mirrors PatchWriter::closeDeferredStrokePatch().
fn (mut w StrokeWriter) close_deferred_stroke_patch(cap StrokeCap) {
	w.write_line(w.last_point, w.first_point)
	if w.has_pending() {
		w.write_deferred_stroke_patch(w.first_point, none)
	} else {
		w.write_deferred_stroke_patch(w.first_point, cap)
	}
}

// conic_has_cusp mirrors ConicHasCusp().
fn conic_has_cusp(p0 Point, p1 Point, p2 Point) bool {
	ax := p1.x - p0.x
	ay := p1.y - p0.y
	bx := p2.x - p1.x
	by := p2.y - p1.y
	c1 := ax * by
	c2 := ay * bx
	d1 := ax * bx
	d2 := ay * by
	return c1 - c2 == 0 && d1 + d2 < 0
}

// write_stroke_patches mirrors TessellateStrokesRenderStep::writeVertices().
fn write_stroke_patches(mut w StrokeWriter, path Path, cap StrokeCap) ! {
	mut pi := 0
	mut wi := 0
	mut last := Point{}
	for verb in path.verbs {
		match verb {
			.move {
				w.write_deferred_stroke_patch(path.points[pi], cap)
				last = path.points[pi]
				pi++
			}
			.close {
				w.close_deferred_stroke_patch(cap)
			}
			.line {
				w.write_line(last, path.points[pi])
				last = path.points[pi]
				pi++
			}
			.quad {
				p1 := path.points[pi]
				p2 := path.points[pi + 1]
				if conic_has_cusp(last, p1, p2) {
					return error('gg.canvas: stroked curves with cusps are not supported')
				}
				w.write_quadratic(last, p1, p2)!
				last = p2
				pi += 2
			}
			.conic {
				p1 := path.points[pi]
				p2 := path.points[pi + 1]
				if conic_has_cusp(last, p1, p2) {
					return error('gg.canvas: stroked curves with cusps are not supported')
				}
				w.write_conic(last, p1, p2, path.weights[wi])!
				last = p2
				pi += 2
				wi++
			}
			.cubic {
				pts := [last, path.points[pi], path.points[pi + 1], path.points[pi + 2]]!
				t, n, cusps := find_cubic_convex_180_chops(pts)
				if n == 0 {
					w.write_cubic(pts[0], pts[1], pts[2], pts[3])!
				} else if n == 1 {
					mut c := chop_cubic_at(pts, t[0])
					if cusps {
						w.write_circle(c[3])
						c[2] = c[3]
						c[4] = c[3]
					}
					w.write_cubic(c[0], c[1], c[2], c[3])!
					w.write_cubic(c[3], c[4], c[5], c[6])!
				} else {
					c := chop_cubic_at2(pts, t[0], t[1])
					if cusps {
						w.write_circle(c[3])
						w.write_circle(c[6])
						w.write_line(c[0], c[3])
						w.write_line(c[3], c[6])
						w.write_line(c[6], c[9])
					} else {
						w.write_cubic(c[0], c[1], c[2], c[3])!
						w.write_cubic(c[3], c[4], c[5], c[6])!
						w.write_cubic(c[6], c[7], c[8], c[9])!
					}
				}
				last = pts[3]
				pi += 3
			}
		}
	}
	w.write_deferred_stroke_patch(Point{0, 0}, cap)
}

fn cross2(a Point, b Point) f32 {
	x := a.x * b.y
	y := a.y * b.x
	return x - y
}

// find_cubic_convex_180_chops mirrors FindCubicConvex180Chops(): up to two
// parameters at which to chop a cubic so its pieces are convex and rotate at
// most 180 degrees, and whether they are cusps.
fn find_cubic_convex_180_chops(pts [4]Point) ([2]f32, int, bool) {
	epsilon := f32(1) / f32(1 << 11)
	one_minus_2_epsilon_bits := u32((127 << 23) - 2 * (1 << (24 - 11)))
	p0, p1, p2, p3 := pts[0], pts[1], pts[2], pts[3]
	c := Point{p1.x - p0.x, p1.y - p0.y}
	d := Point{p2.x - p1.x, p2.y - p1.y}
	e := Point{p3.x - p0.x, p3.y - p0.y}
	b := Point{d.x - c.x, d.y - c.y}
	m3x := f32(-3) * d.x
	m3y := f32(-3) * d.y
	a := Point{m3x + e.x, m3y + e.y}
	mut qa := cross2(a, b)
	qb := cross2(a, c)
	mut qc := cross2(b, c)
	mut b_over_minus_2 := f32(-0.5) * qb
	bb := b_over_minus_2 * b_over_minus_2
	ac := qa * qc
	mut discr_over_4 := bb - ac
	mut cusp_threshold := qa * (epsilon / f32(2))
	cusp_threshold *= cusp_threshold
	inside := fn [epsilon, one_minus_2_epsilon_bits] (root f32) bool {
		return math.f32_bits(root - epsilon) < one_minus_2_epsilon_bits
	}
	mut t := [2]f32{}
	if discr_over_4 < -cusp_threshold {
		root := qc / b_over_minus_2
		if inside(root) {
			t[0] = root
			return t, 1, false
		}
		return t, 0, false
	}
	are_cusps := discr_over_4 <= cusp_threshold
	if are_cusps {
		if qa != 0 || b_over_minus_2 != 0 || qc != 0 {
			root := b_over_minus_2 / qa
			if inside(root) {
				t[0] = root
				return t, 1, true
			}
			return t, 0, true
		}
		tan0 := Point{if c.x != 0 { c.x } else { p2.x - p0.x }, if c.y != 0 {
			c.y
		} else {
			p2.y - p0.y
		}}
		qa = dot2(tan0, a)
		b_over_minus_2 = -dot2(tan0, b)
		qc = dot2(tan0, c)
		bb2 := b_over_minus_2 * b_over_minus_2
		ac2 := qa * qc
		discr_over_4 = bb2 - ac2
		if discr_over_4 < -cusp_threshold {
			return t, 0, true
		}
		discr_over_4 = max_f32(discr_over_4, 0)
	}
	mut q := C.sqrtf(discr_over_4)
	q = f32(math.copysign(f64(q), f64(b_over_minus_2)))
	q = q + b_over_minus_2
	r0 := q / qa
	r1 := qc / q
	in0 := r0 > epsilon && r0 < f32(1) - epsilon
	in1 := r1 > epsilon && r1 < f32(1) - epsilon
	if in0 {
		if in1 && r0 != r1 {
			if r0 > r1 {
				t = [r1, r0]!
			} else {
				t = [r0, r1]!
			}
			return t, 2, are_cusps
		}
		t[0] = r0
		return t, 1, are_cusps
	}
	if in1 {
		t[0] = r1
		return t, 1, are_cusps
	}
	return t, 0, are_cusps
}

// chop_cubic_at mirrors SkChopCubicAt(src, dst[7], t).
fn chop_cubic_at(p [4]Point, t f32) [7]Point {
	if t == 1 {
		return [p[0], p[1], p[2], p[3], p[3], p[3], p[3]]!
	}
	ab := mix_point(p[0], p[1], t)
	bc := mix_point(p[1], p[2], t)
	cd := mix_point(p[2], p[3], t)
	abc := mix_point(ab, bc, t)
	bcd := mix_point(bc, cd, t)
	abcd := mix_point(abc, bcd, t)
	return [p[0], ab, abc, abcd, bcd, cd, p[3]]!
}

// chop_cubic_at2 mirrors SkChopCubicAt(src, dst[10], t0, t1).
fn chop_cubic_at2(p [4]Point, t0 f32, t1 f32) [10]Point {
	if t1 == 1 {
		c := chop_cubic_at(p, t0)
		return [c[0], c[1], c[2], c[3], c[4], c[5], c[6], p[3], p[3], p[3]]!
	}
	ab0 := mix_point(p[0], p[1], t0)
	bc0 := mix_point(p[1], p[2], t0)
	cd0 := mix_point(p[2], p[3], t0)
	abc0 := mix_point(ab0, bc0, t0)
	bcd0 := mix_point(bc0, cd0, t0)
	abcd0 := mix_point(abc0, bcd0, t0)
	ab1 := mix_point(p[0], p[1], t1)
	bc1 := mix_point(p[1], p[2], t1)
	cd1 := mix_point(p[2], p[3], t1)
	abc1 := mix_point(ab1, bc1, t1)
	bcd1 := mix_point(bc1, cd1, t1)
	abcd1 := mix_point(abc1, bcd1, t1)
	middle0 := mix_point(abc0, bcd0, t1)
	middle1 := mix_point(abc1, bcd1, t0)
	return [p[0], ab0, abc0, abcd0, middle0, middle1, abcd1, bcd1, cd1, p[3]]!
}

// stroke_uniforms mirrors TessellateStrokesRenderStep::writeUniformsAndTextures()
// after the paint color: the 2x2 (column-major), translation and the
// transform's largest scale factor.
fn stroke_uniforms(color Color4f, t Transform) PaintUniforms {
	v := t.matrix.m
	return PaintUniforms{
		color:           color
		local_to_device: true
		with_color:      true
		stroke_xform:    true
		matrix:          [v[0], v[3], v[1], v[4], v[2], v[5], t.max_scale, 0, 0, 0, 0, 0, 0, 0,
			0, 0]!
	}
}

// draw_stroke mirrors Device::drawGeometry() for a stroke drawn with the
// tessellated stroke renderer: `path` is the geometry as a path and `shape`
// the shape the clip stack sees.
fn (mut s GpuSurface) draw_stroke(path Path, shape ClipShape, paint Paint) ! {
	if !s.transform.valid() {
		return
	}
	if int(s.transform.typ) > int(TransformType.affine) {
		return error('gg.canvas: perspective transforms are not supported')
	}
	clip := s.visit_clip_stack_for_draw(shape, paint)!
	if clip.clipped_out {
		return
	}
	if !clip.analytic.is_empty() {
		return error('gg.canvas: analytic clips on tessellated strokes are not supported')
	}
	color := paint.color.premul()
	depends_on_dst := !color.is_opaque()
	s.depth_index++
	mut order := DrawOrder{
		depth: s.depth_index
	}
	clip_order := s.update_clip_state_for_draw(clip.elements, clip.draw_bounds, order.depth)
	order.depends_on_paint_order(clip_order)
	if depends_on_dst {
		order.depends_on_paint_order(s.bounds.most_recent_draw(clip.draw_bounds))
	}
	join_type := match paint.join {
		.miter { paint.miter_limit }
		.round { f32(-1) }
		.bevel { f32(0) }
	}
	mut w := new_stroke_writer(s.transform.matrix, s.transform.max_scale, paint.stroke_width * f32(0.5),
		join_type, depth_as_float(order.depth))
	write_stroke_patches(mut w, path, paint.cap)!
	s.record_step(.tessellate_strokes, depends_on_dst, w.out, stroke_uniforms(color, s.transform),
		order, 0, 0, DrawClip{
			scissor:      clip.scissor
			draw_bounds:  clip.draw_bounds
			shape_bounds: clip.shape_bounds
		}, u32(w.count))
	s.draws[s.draws.len - 1].template_count = stroke_vertex_count(w.template)
	s.bounds.record_draw(clip.draw_bounds, order.paint_order)
}
