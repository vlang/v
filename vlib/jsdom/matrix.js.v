module jsdom

pub interface JS.DOMMatrix {
	is_2d JS.Boolean
	is_identity JS.Boolean
mut:
	m11 JS.Number
	m12 JS.Number
	m13 JS.Number
	m14 JS.Number
	m21 JS.Number
	m22 JS.Number
	m23 JS.Number
	m24 JS.Number
	m31 JS.Number
	m32 JS.Number
	m33 JS.Number
	m34 JS.Number
	m41 JS.Number
	m42 JS.Number
	m43 JS.Number
	m44 JS.Number
	a JS.Number
	b JS.Number
	c JS.Number
	d JS.Number
	e JS.Number
	f JS.Number
}

pub struct DOMMatrix {
mut:
	matrix JS.DOMMatrix = JS.DOMMatrix(voidptr(0))
}

pub fn (matrix DOMMatrix) str() string {
	fmt := ''
	#fmt.str = matrix.matrix + ''

	return fmt
}

pub fn new_matrix(init []f64) DOMMatrix {
	#let tmp = new Array();

	for val in init {
		_ := val
		#tmp.push(val);
	}
	mut m := JS.DOMMatrix(voidptr(0))
	#m = new DOMMatrix(tmp);

	return DOMMatrix{m}
}

pub fn (m DOMMatrix) invert_self() {
	#m.matrix.invertSelf();
}

pub fn (m DOMMatrix) multiply_self(other DOMMatrix) {
	#m.matrix.multiplySelf(other.matrix);
}

pub fn (m DOMMatrix) pre_multiply_self(other DOMMatrix) {
	#m.matrix.preMultiplySelf(other.matrix);
}

pub fn (m DOMMatrix) translate_self(tx f64, ty f64, tz f64) {
	#m.matrix.translateSelf(tx.val,ty.val,tz.val);
}

pub fn (m DOMMatrix) scale3d_self(scale f64, origin_x f64, origin_y f64, origin_z f64) {
	#m.matrix.scale3dSelf(scale.val,origin_x.val,origin_y.val,origin_z.val)
}

pub fn (m DOMMatrix) scale_self(scale_x f64, scale_y f64, scale_z f64, origin_x f64, origin_y f64, origin_z f64) {
	#m.matrix.scaleSelf(scale_x.val,scale_y.val,scale_z.val,origin_x.val,origin_y.val,origin_z.val);
}

pub fn (m DOMMatrix) rotate_self(rot_x f64, rot_y f64, rot_z f64) {
	#m.matrix.rotateSelf(rot_x.val,rot_y.val,rot_z.val);
}

pub fn (m DOMMatrix) rotate_axis_angle_self(x f64, y f64, z f64, angle f64) {
	#m.matrix.rotateAxisAngleSelf(x.val,y.val,z.val,angle.val);
}

pub fn (m DOMMatrix) rotate_from_vector_self(x f64, y f64) {
	#m.matrix.rotateFromVectorSelf(x.val,y.val);
}

pub fn (m DOMMatrix) skew_x_self(sx f64) {
	#m.matrix.skewXSelf(sx.val);
}

pub fn (m DOMMatrix) skew_y_self(sy f64) {
	#m.matrix.skewYSelf(sy.val);
}

pub fn (m DOMMatrix) flip_x() DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.flipX();

	return res
}

pub fn (m DOMMatrix) flip_y() DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.flipY();

	return res
}

pub fn (m DOMMatrix) inverse() DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.inverse();

	return res
}

pub fn (m DOMMatrix) multiply(other DOMMatrix) DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.multiply(other.matrix);

	return res
}

pub fn (m DOMMatrix) rotate(rot_x f64, rot_y f64, rot_z f64) DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.rotate(rot_x.val,rot_y.val,rot_z.val);

	return res
}

pub fn (m DOMMatrix) rotate_axis_angle(x f64, y f64, z f64, angle f64) DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.rotateAxisAngle(x.val,y.val,z.val,angle.val);

	return res
}

pub fn (m DOMMatrix) rotate_from_vector(x f64, y f64) DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.rotateFromVector(x.val,y.val);

	return res
}

pub fn (m DOMMatrix) scale(scale_x f64, scale_y f64, scale_z f64, origin_x f64, origin_y f64, origin_z f64) DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.scale(scale_x.val,scale_y.val,scale_z.val,origin_x.val,origin_y.val,origin_z.val);

	return res
}

pub fn (m DOMMatrix) scale3d(scale f64, origin_x f64, origin_y f64, origin_z f64) DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.scale3d(scale.val, origin_x.val,origin_y.val,origin_z.val);

	return res
}

pub fn (m DOMMatrix) skew_x(sx f64) DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.skewX(sx.val);

	return res
}

pub fn (m DOMMatrix) skew_y(sy f64) DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.skewY(sy.val);

	return res
}

pub fn (m DOMMatrix) translate(tx f64, ty f64, tz f64) DOMMatrix {
	res := DOMMatrix{}
	#res.matrix = m.matrix.translate(tx.val,ty.val,tz.val);

	return res
}

pub fn (m DOMMatrix) is_2d() bool {
	res := false
	#res.val = m.matrix.is2D.val;

	return res
}

pub fn (m DOMMatrix) a() f64 {
	return f64(m.matrix.a)
}

pub fn (m DOMMatrix) b() f64 {
	return f64(m.matrix.b)
}

pub fn (m DOMMatrix) c() f64 {
	return f64(m.matrix.c)
}

pub fn (m DOMMatrix) d() f64 {
	return f64(m.matrix.d)
}

pub fn (m DOMMatrix) e() f64 {
	return f64(m.matrix.e)
}

pub fn (m DOMMatrix) f() f64 {
	return f64(m.matrix.f)
}

pub fn (mut m DOMMatrix) set_a(a f64) {
	m.matrix.a = JS.Number(a)
}

pub fn (mut m DOMMatrix) set_b(b f64) {
	m.matrix.b = JS.Number(b)
}

pub fn (mut m DOMMatrix) set_c(c f64) {
	m.matrix.c = JS.Number(c)
}

pub fn (mut m DOMMatrix) set_d(d f64) {
	m.matrix.d = JS.Number(d)
}

pub fn (mut m DOMMatrix) set_e(e f64) {
	m.matrix.e = JS.Number(e)
}

pub fn (mut m DOMMatrix) set_f(f f64) {
	m.matrix.f = JS.Number(f)
}

pub fn (m DOMMatrix) m11() f64 {
	return f64(m.matrix.m11)
}

pub fn (m DOMMatrix) m12() f64 {
	return f64(m.matrix.m12)
}

pub fn (m DOMMatrix) m13() f64 {
	return f64(m.matrix.m13)
}

pub fn (m DOMMatrix) m14() f64 {
	return f64(m.matrix.m14)
}

pub fn (m DOMMatrix) m21() f64 {
	return f64(m.matrix.m21)
}

pub fn (m DOMMatrix) m22() f64 {
	return f64(m.matrix.m22)
}

pub fn (m DOMMatrix) m23() f64 {
	return f64(m.matrix.m23)
}

pub fn (m DOMMatrix) m24() f64 {
	return f64(m.matrix.m24)
}

pub fn (m DOMMatrix) m31() f64 {
	return f64(m.matrix.m31)
}

pub fn (m DOMMatrix) m32() f64 {
	return f64(m.matrix.m32)
}

pub fn (m DOMMatrix) m33() f64 {
	return f64(m.matrix.m33)
}

pub fn (m DOMMatrix) m34() f64 {
	return f64(m.matrix.m34)
}

pub fn (m DOMMatrix) m41() f64 {
	return f64(m.matrix.m41)
}

pub fn (m DOMMatrix) m42() f64 {
	return f64(m.matrix.m42)
}

pub fn (m DOMMatrix) m43() f64 {
	return f64(m.matrix.m43)
}

pub fn (m DOMMatrix) m44() f64 {
	return f64(m.matrix.m44)
}

pub fn (mut m DOMMatrix) set_m11(x f64) {
	m.matrix.m11 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m12(x f64) {
	m.matrix.m12 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m13(x f64) {
	m.matrix.m13 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m14(x f64) {
	m.matrix.m14 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m21(x f64) {
	m.matrix.m21 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m22(x f64) {
	m.matrix.m22 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m23(x f64) {
	m.matrix.m23 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m24(x f64) {
	m.matrix.m24 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m31(x f64) {
	m.matrix.m31 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m32(x f64) {
	m.matrix.m32 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m33(x f64) {
	m.matrix.m33 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m34(x f64) {
	m.matrix.m34 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m41(x f64) {
	m.matrix.m41 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m42(x f64) {
	m.matrix.m42 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m43(x f64) {
	m.matrix.m43 = JS.Number(x)
}

pub fn (mut m DOMMatrix) set_m44(x f64) {
	m.matrix.m44 = JS.Number(x)
}
