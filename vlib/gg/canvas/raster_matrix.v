module canvas

// RasterMatrix reproduces SkMatrix: the same type-mask bookkeeping and the
// same float operations for concatenation and point mapping. The CPU raster
// and text code depends on both (type masks select the arithmetic).

const mat_scale_x = 0
const mat_skew_x = 1
const mat_trans_x = 2
const mat_skew_y = 3
const mat_scale_y = 4
const mat_trans_y = 5
const mat_persp_0 = 6
const mat_persp_1 = 7
const mat_persp_2 = 8

const type_identity = u8(0)
const type_translate = u8(0x01)
const type_scale = u8(0x02)
const type_affine = u8(0x04)
const type_perspective = u8(0x08)
const type_rect_stays_rect = u8(0x10)
const type_only_perspective_valid = u8(0x40)
const type_unknown = u8(0x80)

struct RasterMatrix {
mut:
	m         [9]f32
	type_mask u8
}

fn RasterMatrix.identity() RasterMatrix {
	return RasterMatrix{
		m:         [f32(1), 0, 0, 0, 1, 0, 0, 0, 1]!
		type_mask: type_identity | type_rect_stays_rect
	}
}

// RasterMatrix.from creates a matrix like SkMatrix::setAll(): the type is
// computed on first use.
fn RasterMatrix.from(m Matrix) RasterMatrix {
	return RasterMatrix{
		m:         m.m
		type_mask: type_unknown
	}
}

// RasterMatrix.affine mirrors SkMatrix::MakeAll(a, c, e, b, d, f, 0, 0, 1).
fn RasterMatrix.affine(a f32, b f32, c f32, d f32, e f32, f f32) RasterMatrix {
	return RasterMatrix{
		m:         [a, c, e, b, d, f, 0, 0, 1]!
		type_mask: type_unknown
	}
}

fn RasterMatrix.translate(dx f32, dy f32) RasterMatrix {
	return RasterMatrix{
		m:         [f32(1), 0, dx, 0, 1, dy, 0, 0, 1]!
		type_mask: if dx != 0 || dy != 0 {
			type_translate | type_rect_stays_rect
		} else {
			type_identity | type_rect_stays_rect
		}
	}
}

// RasterMatrix.scale mirrors SkMatrix::setScale(sx, sy).
fn RasterMatrix.scale(sx f32, sy f32) RasterMatrix {
	rect_mask := if sx == 0 || sy == 0 { u8(0) } else { type_rect_stays_rect }
	return RasterMatrix{
		m:         [sx, f32(0), 0, 0, sy, 0, 0, 0, 1]!
		type_mask: if sx == 1 && sy == 1 {
			type_identity | rect_mask
		} else {
			type_scale | rect_mask
		}
	}
}

// RasterMatrix.scale_translate mirrors SkMatrix::ScaleTranslate().
fn RasterMatrix.scale_translate(sx f32, sy f32, tx f32, ty f32) RasterMatrix {
	mut mask := u8(0)
	if sx != 1 || sy != 1 {
		mask |= type_scale
	}
	if tx != 0 || ty != 0 {
		mask |= type_translate
	}
	if sx != 0 && sy != 0 {
		mask |= type_rect_stays_rect
	}
	return RasterMatrix{
		m:         [sx, f32(0), tx, 0, sy, ty, 0, 0, 1]!
		type_mask: mask
	}
}

// RasterMatrix.skew mirrors SkMatrix::setSkew(sx, sy).
fn RasterMatrix.skew(sx f32, sy f32) RasterMatrix {
	return RasterMatrix{
		m:         [f32(1), sx, 0, sy, 1, 0, 0, 0, 1]!
		type_mask: type_unknown | type_only_perspective_valid
	}
}

fn (m &RasterMatrix) compute_type_mask() u8 {
	v := m.m
	if v[mat_persp_0] != 0 || v[mat_persp_1] != 0 || v[mat_persp_2] != 1 {
		return type_translate | type_scale | type_affine | type_perspective
	}
	mut mask := u8(0)
	if v[mat_trans_x] != 0 || v[mat_trans_y] != 0 {
		mask |= type_translate
	}
	// SkScalarAs2sCompliment distinguishes -0 from 0 only through the sign
	// bit, which it folds away: both compare equal to 0 here as well.
	m00 := as_2s_complement(v[mat_scale_x])
	m01 := as_2s_complement(v[mat_skew_x])
	m10 := as_2s_complement(v[mat_skew_y])
	m11 := as_2s_complement(v[mat_scale_y])
	if m01 | m10 != 0 {
		mask |= type_affine | type_scale
		dp0 := m00 | m11 == 0
		ds1 := m01 != 0 && m10 != 0
		if dp0 && ds1 {
			mask |= type_rect_stays_rect
		}
	} else {
		one := as_2s_complement(f32(1))
		if (m00 ^ one) | (m11 ^ one) != 0 {
			mask |= type_scale
		}
		if m00 != 0 && m11 != 0 {
			mask |= type_rect_stays_rect
		}
	}
	return mask
}

// as_2s_complement mirrors SkScalarAs2sCompliment: the float bits as a
// two's complement integer, so -0 and 0 both become 0.
fn as_2s_complement(x f32) i32 {
	bits := i32(math_f32_bits(x))
	if bits < 0 {
		return -(bits & 0x7fffffff)
	}
	return bits
}

fn math_f32_bits(x f32) u32 {
	return unsafe { *(&u32(&x)) }
}

fn (mut m RasterMatrix) get_type() u8 {
	if m.type_mask & type_unknown != 0 {
		m.type_mask = m.compute_type_mask()
	}
	return m.type_mask & 0xf
}

fn (m &RasterMatrix) is_trivially_identity() bool {
	if m.type_mask & type_unknown != 0 {
		return false
	}
	return m.type_mask & 0xf == 0
}

fn (m &RasterMatrix) has_perspective() bool {
	return m.m[mat_persp_0] != 0 || m.m[mat_persp_1] != 0 || m.m[mat_persp_2] != 1
}

fn (mut m RasterMatrix) is_identity() bool {
	return m.get_type() == 0
}

fn (mut m RasterMatrix) update_translate_mask() {
	if m.m[mat_trans_x] != 0 || m.m[mat_trans_y] != 0 {
		m.type_mask |= type_translate
	} else {
		m.type_mask &= ~type_translate
	}
}

// sdot computes `a * b + c * d` without fusing, like Skia's sdot().
@[inline]
fn sdot(a f32, b f32, c f32, d f32) f32 {
	ab := a * b
	cd := c * d
	return ab + cd
}

// muladdmul mirrors SkMatrix.cpp's muladdmul: the sum of products in double.
@[inline]
fn muladdmul(a f32, b f32, c f32, d f32) f32 {
	ab := f64(a) * f64(b)
	cd := f64(c) * f64(d)
	return f32(ab + cd)
}

fn (mut m RasterMatrix) pre_translate(dx f32, dy f32) {
	mask := m.get_type()
	if mask <= type_translate {
		m.m[mat_trans_x] += dx
		m.m[mat_trans_y] += dy
	} else if mask & type_perspective != 0 {
		m.pre_concat(RasterMatrix.translate(dx, dy))
		return
	} else {
		tx := sdot(m.m[mat_scale_x], dx, m.m[mat_skew_x], dy)
		ty := sdot(m.m[mat_skew_y], dx, m.m[mat_scale_y], dy)
		m.m[mat_trans_x] += tx
		m.m[mat_trans_y] += ty
	}
	m.update_translate_mask()
}

fn (mut m RasterMatrix) post_translate(dx f32, dy f32) {
	if m.has_perspective() {
		m.post_concat(RasterMatrix.translate(dx, dy))
		return
	}
	m.m[mat_trans_x] += dx
	m.m[mat_trans_y] += dy
	m.update_translate_mask()
}

// pre_scale mirrors SkMatrix::preScale(sx, sy).
fn (mut m RasterMatrix) pre_scale(sx f32, sy f32) {
	if sx == 1 && sy == 1 {
		return
	}
	m.m[mat_scale_x] *= sx
	m.m[mat_skew_y] *= sx
	m.m[mat_persp_0] *= sx
	m.m[mat_skew_x] *= sy
	m.m[mat_scale_y] *= sy
	m.m[mat_persp_1] *= sy
	if m.m[mat_scale_x] == 1 && m.m[mat_scale_y] == 1
		&& m.type_mask & (type_perspective | type_affine) == 0 {
		m.type_mask &= ~type_scale
	} else {
		m.type_mask |= type_scale
		if sx == 0 || sy == 0 {
			m.type_mask &= ~type_rect_stays_rect
		}
	}
}

fn (m &RasterMatrix) concat_with(b RasterMatrix) RasterMatrix {
	mut a := *m
	return set_concat(mut a, b)
}

// set_concat mirrors SkMatrix::setConcat(a, b) and returns a * b.
fn set_concat(mut a RasterMatrix, b_ RasterMatrix) RasterMatrix {
	mut b := b_
	a_type := a.get_type()
	b_type := b.get_type()
	if a.is_trivially_identity() {
		return b
	}
	if b.is_trivially_identity() {
		return a
	}
	am := a.m
	bm := b.m
	if (a_type | b_type) & (type_affine | type_perspective) == 0 {
		sx := am[mat_scale_x] * bm[mat_scale_x]
		sy := am[mat_scale_y] * bm[mat_scale_y]
		tx0 := am[mat_scale_x] * bm[mat_trans_x]
		ty0 := am[mat_scale_y] * bm[mat_trans_y]
		return RasterMatrix.scale_translate(sx, sy, tx0 + am[mat_trans_x], ty0 + am[mat_trans_y])
	}
	if (a_type | b_type) & type_perspective != 0 {
		mut t := RasterMatrix{
			type_mask: type_unknown
		}
		for row in 0 .. 3 {
			for col in 0 .. 3 {
				p0 := am[row * 3] * bm[col]
				p1 := am[row * 3 + 1] * bm[3 + col]
				p2 := am[row * 3 + 2] * bm[6 + col]
				s := p0 + p1
				t.m[row * 3 + col] = s + p2
			}
		}
		return t
	}
	mut t := RasterMatrix{
		type_mask: type_unknown | type_only_perspective_valid
	}
	t.m[mat_scale_x] = muladdmul(am[mat_scale_x], bm[mat_scale_x], am[mat_skew_x], bm[mat_skew_y])
	t.m[mat_skew_x] = muladdmul(am[mat_scale_x], bm[mat_skew_x], am[mat_skew_x], bm[mat_scale_y])
	tx := muladdmul(am[mat_scale_x], bm[mat_trans_x], am[mat_skew_x], bm[mat_trans_y])
	t.m[mat_trans_x] = tx + am[mat_trans_x]
	t.m[mat_skew_y] = muladdmul(am[mat_skew_y], bm[mat_scale_x], am[mat_scale_y], bm[mat_skew_y])
	t.m[mat_scale_y] = muladdmul(am[mat_skew_y], bm[mat_skew_x], am[mat_scale_y], bm[mat_scale_y])
	ty := muladdmul(am[mat_skew_y], bm[mat_trans_x], am[mat_scale_y], bm[mat_trans_y])
	t.m[mat_trans_y] = ty + am[mat_trans_y]
	t.m[mat_persp_0] = 0
	t.m[mat_persp_1] = 0
	t.m[mat_persp_2] = 1
	return t
}

// pre_concat sets this matrix to `this * other`.
fn (mut m RasterMatrix) pre_concat(other RasterMatrix) {
	mut o := other
	if !o.is_identity() {
		result := set_concat(mut m, o)
		m.m = result.m
		m.type_mask = result.type_mask
	}
}

// post_concat sets this matrix to `other * this`.
fn (mut m RasterMatrix) post_concat(other RasterMatrix) {
	mut o := other
	if !o.is_identity() {
		result := set_concat(mut o, *m)
		m.m = result.m
		m.type_mask = result.type_mask
	}
}

// map_point mirrors SkMatrix::mapPoint() for affine matrices.
fn (m &RasterMatrix) map_point(p Point) Point {
	v := m.m
	x0 := p.x * v[0]
	x1 := p.y * v[1]
	y0 := p.x * v[3]
	y1 := p.y * v[4]
	xs := x0 + x1
	ys := y0 + y1
	return Point{xs + v[2], ys + v[5]}
}

// map_points mirrors SkMatrix::mapPoints(): the type selects the procedure,
// and the vector procedures compute `p * scale + trans` per lane.
fn (mut m RasterMatrix) map_points(pts []Point) []Point {
	typ := m.get_type()
	v := m.m
	mut out := []Point{len: pts.len}
	if typ == 0 {
		return pts.clone()
	}
	if typ == type_translate {
		for i, p in pts {
			out[i] = Point{p.x + v[mat_trans_x], p.y + v[mat_trans_y]}
		}
		return out
	}
	if typ & (type_affine | type_perspective) == 0 {
		for i, p in pts {
			sx := p.x * v[mat_scale_x]
			sy := p.y * v[mat_scale_y]
			out[i] = Point{sx + v[mat_trans_x], sy + v[mat_trans_y]}
		}
		return out
	}
	// Affine_vpts: src * scale + swizzle(src) * skew + trans, left to right.
	for i, p in pts {
		ax := p.x * v[mat_scale_x]
		ay := p.y * v[mat_scale_y]
		bx := p.y * v[mat_skew_x]
		by := p.x * v[mat_skew_y]
		cx := ax + bx
		cy := ay + by
		out[i] = Point{cx + v[mat_trans_x], cy + v[mat_trans_y]}
	}
	return out
}

// map_rect mirrors SkMatrix::mapRect() for affine matrices.
fn (mut m RasterMatrix) map_rect(r Rect) Rect {
	typ := m.get_type()
	v := m.m
	if typ <= type_translate {
		tx := v[mat_trans_x]
		ty := v[mat_trans_y]
		return Rect.ltrb(r.left() + tx, r.top() + ty, r.right() + tx, r.bottom() + ty).sorted()
	}
	if typ & (type_affine | type_perspective) == 0 {
		l := r.left() * v[mat_scale_x]
		t := r.top() * v[mat_scale_y]
		rr := r.right() * v[mat_scale_x]
		b := r.bottom() * v[mat_scale_y]
		return Rect.ltrb(l + v[mat_trans_x], t + v[mat_trans_y], rr + v[mat_trans_x],
			b + v[mat_trans_y]).sorted()
	}
	quad := m.map_points([Point{r.left(), r.top()}, Point{r.right(), r.top()},
		Point{r.right(), r.bottom()}, Point{r.left(), r.bottom()}])
	mut l := quad[0].x
	mut t := quad[0].y
	mut rr := quad[0].x
	mut b := quad[0].y
	for q in quad[1..] {
		l = min_f32(l, q.x)
		t = min_f32(t, q.y)
		rr = max_f32(rr, q.x)
		b = max_f32(b, q.y)
	}
	return Rect.ltrb(l, t, rr, b)
}
