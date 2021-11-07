module jsdom

pub struct JS.DOMPoint {
pub mut:
	x JS.Number [noinit]
	y JS.Number [noinit]
	z JS.Number [noinit]
	w JS.Number [noinit]
}

pub struct DOMPoint {
mut:
	point JS.DOMPoint
}

pub fn new_dompoint(x f64, y f64, z f64, w f64) DOMPoint {
	mut point := DOMPoint{}

	point.point.x = f64tonum(x)
	point.point.y = f64tonum(y)
	point.point.z = f64tonum(z)
	point.point.w = f64tonum(w)

	return point
}

pub fn (p DOMPoint) x() f64 {
	return tof64(p.point.x)
}

pub fn (p DOMPoint) y() f64 {
	return tof64(p.point.y)
}

pub fn (p DOMPoint) z() f64 {
	return tof64(p.point.z)
}

pub fn (p DOMPoint) w() f64 {
	return tof64(p.point.w)
}

pub fn (mut p DOMPoint) set_x(x f64) {
	p.point.x = f64tonum(x)
}

pub fn (mut p DOMPoint) set_y(y f64) {
	p.point.y = f64tonum(y)
}

pub fn (mut p DOMPoint) set_z(z f64) {
	p.point.z = f64tonum(z)
}

pub fn (mut p DOMPoint) set_w(w f64) {
	p.point.w = f64tonum(w)
}

pub fn (p DOMPoint) matrix_transform(matrix DOMMatrix) DOMPoint {
	mut point := DOMPoint{}
	#point.point = p.point.matrixTransform(matrix.matrix);

	return point
}

pub fn (p DOMPoint) matrix_transform_2() DOMPoint {
	mut point := DOMPoint{}
	#point.point = p.point.matrixTransform();

	return point
}
