module canvas

import math

// Matrix is a 3x3 transform in SkMatrix's row-major order:
// `[scale_x, skew_x, trans_x, skew_y, scale_y, trans_y, persp_0, persp_1, persp_2]`.
//
// Create matrices with the constructors; the zero value `Matrix{}` is not a
// valid transform.
pub struct Matrix {
pub:
	m [9]f32
}

pub fn Matrix.identity() Matrix {
	return Matrix{
		m: [f32(1), 0, 0, 0, 1, 0, 0, 0, 1]!
	}
}

pub fn Matrix.translate(dx f32, dy f32) Matrix {
	return Matrix{
		m: [f32(1), 0, dx, 0, 1, dy, 0, 0, 1]!
	}
}

pub fn Matrix.scale(sx f32, sy f32) Matrix {
	return Matrix{
		m: [sx, f32(0), 0, 0, sy, 0, 0, 0, 1]!
	}
}

// Matrix.values creates a matrix from nine row-major values, like SkMatrix::set9().
pub fn Matrix.values(values [9]f32) Matrix {
	return Matrix{
		m: values
	}
}

// TransformType classifies a transform like skgpu::graphite::Transform::Type.
pub enum TransformType {
	identity
	simple_rect_stays_rect // positive scale and translation
	rect_stays_rect        // also axis flips and 90-degree rotations
	affine
	perspective
	invalid
}

// Transform caches the classification, inverse and scale factors of a local to
// device matrix, like Graphite's Transform for an SkM44 created from an SkMatrix.
pub struct Transform {
pub:
	matrix Matrix
	typ    TransformType
	// The inverse, for types up to `.affine`.
	inverse   Matrix
	min_scale f32
	max_scale f32
}

pub fn Transform.new(matrix Matrix) Transform {
	m := matrix.m
	if m[6] != 0 || m[7] != 0 || m[8] != 1 {
		// Perspective transforms are classified but not supported by the
		// renderers yet; scale factors are computed per point by Graphite.
		return Transform{
			matrix: matrix
			typ:    .perspective
		}
	}
	sx := m[0]
	sy := m[4]
	kx := m[1]
	ky := m[3]
	tx := m[2]
	ty := m[5]
	if kx == 0 && ky == 0 {
		if sx == 0 || sy == 0 {
			return Transform{
				matrix: matrix
				typ:    .invalid
			}
		}
		if sx == 1 && sy == 1 && tx == 0 && ty == 0 {
			return Transform{
				matrix:    matrix
				typ:       .identity
				inverse:   Matrix.identity()
				min_scale: 1
				max_scale: 1
			}
		}
		ix := f32(1) / sx
		iy := f32(1) / sy
		min_s, max_s := sort_scale(sx, sy)
		return Transform{
			matrix:    matrix
			typ:       if sx > 0 && sy > 0 { .simple_rect_stays_rect } else { .rect_stays_rect }
			inverse:   Matrix{
				m: [ix, f32(0), -ix * tx, 0, iy, -iy * ty, 0, 0, 1]!
			}
			min_scale: min_s
			max_scale: max_s
		}
	}
	if sx == 0 && sy == 0 {
		if kx == 0 || ky == 0 {
			return Transform{
				matrix: matrix
				typ:    .invalid
			}
		}
		ix := f32(1) / kx
		iy := f32(1) / ky
		min_s, max_s := sort_scale(kx, ky)
		return Transform{
			matrix:    matrix
			typ:       .rect_stays_rect
			inverse:   Matrix{
				m: [f32(0), iy, -iy * ty, ix, 0, -ix * tx, 0, 0, 1]!
			}
			min_scale: min_s
			max_scale: max_s
		}
	}
	// SkInvert2x2Matrix on the column-major upper 2x2 `[sx, ky, kx, sy]`.
	inv := invert_2x2(sx, ky, kx, sy) or {
		return Transform{
			matrix: matrix
			typ:    .invalid
		}
	}
	min_s, max_s := compute_svd(sx, kx, ky, sy)
	return Transform{
		matrix:    matrix
		typ:       .affine
		inverse:   Matrix{
			m: [inv[0], inv[2], -inv[0] * tx - inv[2] * ty, inv[1], inv[3], -inv[1] * tx - inv[3] * ty,
				0, 0, 1]!
		}
		min_scale: min_s
		max_scale: max_s
	}
}

pub fn (t Transform) valid() bool {
	return t.typ != .invalid
}

// local_aa_radius returns the local-space distance that maps to at least one
// device pixel, like Transform::localAARadius() for non-perspective matrices.
pub fn (t Transform) local_aa_radius() f32 {
	radius := f32(1) / t.min_scale
	return if is_finite_f32(radius) { radius } else { f32(math.inf(1)) }
}

// map_rect maps a rectangle to device space.
pub fn (t Transform) map_rect(r Rect) Rect {
	return map_rect_with(t.typ, t.matrix, r)
}

// inverse_map_rect maps a device-space rectangle back to local space. Like
// Graphite it classifies the inverse with the forward transform's type.
pub fn (t Transform) inverse_map_rect(r Rect) Rect {
	return map_rect_with(t.typ, t.inverse, r)
}

fn map_rect_with(typ TransformType, matrix Matrix, r Rect) Rect {
	m := matrix.m
	match typ {
		.identity {
			return r
		}
		.simple_rect_stays_rect {
			return Rect{
				vals: scale_translate_vals(r.vals, m[0], m[4], m[2], m[5])
			}
		}
		.rect_stays_rect {
			if m[0] == 0 {
				v := scale_translate_vals(r.vals, m[3], m[1], m[5], m[2])
				return Rect{
					vals: [v[1], v[0], v[3], v[2]]!
				}.sorted()
			}
			return Rect{
				vals: scale_translate_vals(r.vals, m[0], m[4], m[2], m[5])
			}.sorted()
		}
		.invalid {
			return Rect.infinite_inverted()
		}
		else {
			return map_rect_points(matrix, r)
		}
	}
}

// map_rect_points mirrors map_rect_affine() in SkM44.cpp: the corners are
// transformed without translation, reduced with min() (using negation for the
// maxima) and translated once. Every product and sum is rounded separately,
// as in Skia's skvx vector code compiled with -ffp-contract=off.
fn map_rect_points(matrix Matrix, r Rect) Rect {
	m := matrix.m
	lt := corner_products(m, r.left(), r.top())
	rt := corner_products(m, r.right(), r.top())
	lb := corner_products(m, r.left(), r.bottom())
	rb := corner_products(m, r.right(), r.bottom())
	mut mins := [4]f32{}
	for i in 0 .. 4 {
		mins[i] = min_f32(min_f32(lt[i], rt[i]), min_f32(lb[i], rb[i]))
	}
	// c3 + flip * min(...): the flips are exact negations.
	return Rect.ltrb(m[2] + mins[0], m[5] + mins[1], m[2] + -mins[2], m[5] + -mins[3])
}

// corner_products returns `(x', y', -x', -y')` for the untranslated corner,
// computing each product before the sum.
fn corner_products(m [9]f32, x f32, y f32) [4]f32 {
	ax := m[0] * x
	ay := m[3] * x
	bx := m[1] * y
	by := m[4] * y
	nax := -m[0] * x
	nay := -m[3] * x
	nbx := -m[1] * y
	nby := -m[4] * y
	return [ax + bx, ay + by, nax + nbx, nay + nby]!
}

fn scale_translate_vals(vals [4]f32, sx f32, sy f32, tx f32, ty f32) [4]f32 {
	// Skia multiplies and adds whole skvx vectors, which the compiler never
	// fuses into FMAs; separate statements keep the two roundings. The
	// (-tx, -ty) terms keep the result in [l, t, -r, -b] form.
	scaled := [vals[0] * sx, vals[1] * sy, vals[2] * sx, vals[3] * sy]!
	return [scaled[0] + tx, scaled[1] + ty, scaled[2] + -tx, scaled[3] + -ty]!
}

fn sort_scale(sx f32, sy f32) (f32, f32) {
	min := abs_f32(sx)
	max := abs_f32(sy)
	if min > max {
		return max, min
	}
	return min, max
}

// compute_svd returns the singular values of [m00, m01][m10, m11]. Each
// product is a separate statement so that the C compiler cannot fuse it into
// the following addition (Skia is compiled with -ffp-contract=off).
fn compute_svd(m00 f32, m01 f32, m10 f32, m11 f32) (f32, f32) {
	p00 := m00 * m00
	p01 := m01 * m01
	p10 := m10 * m10
	p11 := m11 * m11
	a := p00 + p01
	b := a + p10
	s1 := b + p11
	c := a - p10
	e := c - p11
	q0 := m00 * m10
	q1 := m01 * m11
	f := q0 + q1
	ee := e * e
	four_f := f32(4) * f
	ff := four_f * f
	s2 := C.sqrtf(ee + ff)
	lo := s1 - s2
	hi := s1 + s2
	return C.sqrtf(f32(0.5) * lo), C.sqrtf(f32(0.5) * hi)
}

// invert_2x2 mirrors SkInvert2x2Matrix, which computes in double precision,
// for a column-major 2x2 matrix.
fn invert_2x2(a f32, b f32, c f32, d f32) ?[4]f32 {
	a00 := f64(a)
	a01 := f64(b)
	a10 := f64(c)
	a11 := f64(d)
	d0 := a00 * a11
	d1 := a01 * a10
	determinant := d0 - d1
	invdet := 1.0 / determinant
	out := [f32(a11 * invdet), f32(-a01 * invdet), f32(-a10 * invdet), f32(a00 * invdet)]!
	for v in out {
		if !is_finite_f32(v) {
			return none
		}
	}
	if determinant == 0 {
		return none
	}
	return out
}

// concat_affine mirrors SkM44::setConcat(a, b) for affine matrices embedded
// in SkM44s: each column of the product is c0*r[0] + (c1*r[1] + (c2*r[2] +
// c3*r[3])) over a's columns c0..c3 and b's column r, with every product and
// sum rounded separately (no FMAs).
fn concat_affine(a Matrix, b Matrix) Matrix {
	am := a.m
	bm := b.m
	mut out := [9]f32{}
	// b's columns as (r0, r1, r3): sx/ky, kx/sy and tx/ty; r2 is 0 for all.
	for j, r in [[bm[0], bm[3], f32(0)], [bm[1], bm[4], f32(0)], [bm[2], bm[5], f32(1)]] {
		// The rows x and y; a's third column is (0, 0, 1, 0).
		for i in 0 .. 2 {
			p0 := am[i * 3] * r[0]
			p1 := am[i * 3 + 1] * r[1]
			p2 := f32(0) * f32(0)
			p3 := am[i * 3 + 2] * r[2]
			s23 := p2 + p3
			s123 := p1 + s23
			out[i * 3 + j] = p0 + s123
		}
	}
	out[8] = 1
	return Matrix{
		m: out
	}
}
