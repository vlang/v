module jsdom

pub struct CanvasRenderingContext2DSettings {
pub mut:
	alpha                bool
	color_space          string
	desynchronized       bool
	will_read_frequently bool
}

pub interface JS.DOMMatrix2DInit {
mut:
	a JS.Number
	b JS.Number
	c JS.Number
	d JS.Number
	e JS.Number
	f JS.Number
	m11 JS.Number
	m12 JS.Number
	m21 JS.Number
	m22 JS.Number
	m41 JS.Number
	m42 JS.Number
}

pub interface JS.DOMMatrixInit {
	JS.DOMMatrix2DInit
	is2D JS.Boolean
mut:
	m13 JS.Number
	m14 JS.Number
	m23 JS.Number
	m24 JS.Number
	m31 JS.Number
	m32 JS.Number
	m33 JS.Number
	m34 JS.Number
	m43 JS.Number
	m44 JS.Number
}

pub interface JS.DOMMatrix {
	is2D JS.Boolean
	isIdentity JS.Boolean
	flipX() JS.DOMMatrix
	flipY() JS.DOMMatrix
	inverse() JS.DOMMatrix
	multiply(other JS.DOMMatrix) JS.DOMMatrix
	rotate(rotX JS.Number, rotY JS.Number, rotZ JS.Number) JS.DOMMatrix
	rotateAxisAngle(x JS.Number, y JS.Number, z JS.Number, angle JS.Number) JS.DOMMatrix
	scale(scaleX JS.Number, scaleY JS.Number, scaleZ JS.Number, originX JS.Number, originY JS.Number, originZ JS.Number) JS.DOMMatrix
	scale3d(scale JS.Number, originX JS.Number, originY JS.Number, originZ JS.Number) JS.DOMMatrix
	skewX(sx JS.Number) JS.DOMMatrix
	skewY(sy JS.Number) JS.DOMMatrix
	translate(tx JS.Number, ty JS.Number, tz JS.Number) JS.DOMMatrix
	invertSelf() JS.DOMMatrix
	multiplySelf(other JS.DOMMatrix) JS.DOMMatrix
	preMultiplySelf(other JS.DOMMatrix) JS.DOMMatrix
	rotateAxisAngleSelf(x JS.Number, y JS.Number, z JS.Number, angle JS.Number) JS.DOMMatrix
	rotateFromVectorSelf(x JS.Number, y JS.Number) JS.DOMMatrix
	rotateSelf(rotX JS.Number, rotY JS.Number, rotZ JS.Number) JS.DOMMatrix
	scale3dSelf(scale JS.Number, originX JS.Number, originY JS.Number, originZ JS.Number) JS.DOMMatrix
	scaleSelf(scaleX JS.Number, scaleY JS.Number, scaleZ JS.Number, originX JS.Number, originY JS.Number, originZ JS.Number) JS.DOMMatrix
	toString() JS.String
mut:
	a JS.Number
	b JS.Number
	c JS.Number
	d JS.Number
	e JS.Number
	f JS.Number
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
}

pub type SVGMatrix = JS.DOMMatrix
pub type WebKitCSSMatrix = JS.DOMMatrix

[use_new]
pub fn JS.DOMMatrix.prototype.constructor(init JS.Array) JS.DOMMatrix

pub interface JS.DOMPoint {
	matrixTransform(matrix JS.DOMMatrix) JS.DOMPoint
mut:
	w JS.Number
	x JS.Number
	y JS.Number
	z JS.Number
}

[use_new]
pub fn JS.DOMPoint.prototype.constructor(x JS.Number, y JS.Number, z JS.Number, w JS.Number) JS.DOMPoint

pub interface JS.DOMQuad {
	p1 JS.DOMPoint
	p2 JS.DOMPoint
	p3 JS.DOMPoint
	p4 JS.DOMPoint
	getBounds() JS.DOMRect
}

[use_new]
pub fn JS.DOMQuad.prototype.constructor(p1 JS.DOMPoint, p2 JS.DOMPoint, p3 JS.DOMPoint, p4 JS.DOMPoint) JS.DOMQuad
pub fn JS.DOMQuad.fromQuad(other JS.DOMQuad) JS.DOMQuad
pub fn JS.DOMQuad.fromRect(other JS.DOMRect) JS.DOMRect

pub interface JS.DOMRect {
	bottom JS.Number
	left JS.Number
	right JS.Number
	top JS.Number
mut:
	height JS.Number
	width JS.Number
	x JS.Number
	y JS.Number
}

pub interface JS.DOMStringList {
	length JS.Number
	contains(JS.String) JS.Boolean
	item(index JS.Number) ?JS.String
}

#function foo() { return null; }

pub fn JS.foo() ?JS.String

pub fn JS.DOMStringList.prototype.constructor() JS.DOMStringList
