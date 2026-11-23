module svg

import math

// Transform is a 2x3 affine matrix: [a c e; b d f; 0 0 1].
pub struct Transform {
pub:
	a f64 = 1
	b f64 = 0
	c f64 = 0
	d f64 = 1
	e f64 = 0
	f f64 = 0
}

// identity returns the identity transform.
pub fn Transform.identity() Transform {
	return Transform{}
}

// translate returns a translation transform.
pub fn Transform.translate(x f64, y f64) Transform {
	return Transform{ e: x, f: y }
}

// scale returns a scaling transform.
pub fn Transform.scale(x f64, y f64) Transform {
	return Transform{ a: x, d: y }
}

// rotate returns a rotation about the origin, in degrees.
pub fn Transform.rotate(deg f64) Transform {
	r := deg * math.pi / 180
	co := math.cos(r)
	si := math.sin(r)
	return Transform{ a: co, b: si, c: -si, d: co }
}

// shear returns a skew transform in degrees (x around y axis, y around x axis).
pub fn Transform.shear(x_deg f64, y_deg f64) Transform {
	return Transform{
		// SVG skewX changes x by tan(a)*y (matrix c); skewY changes y by tan(a)*x (matrix b).
		c: math.tan(x_deg * math.pi / 180)
		b: math.tan(y_deg * math.pi / 180)
	}
}

// from_matrix builds a transform from SVG matrix(a,b,c,d,e,f) components.
pub fn Transform.from_matrix(a f64, b f64, c f64, d f64, e f64, f f64) Transform {
	return Transform{ a: a, b: b, c: c, d: d, e: e, f: f }
}

// then returns self followed by other (self * other), i.e. other is applied first.
pub fn (t Transform) then(other Transform) Transform {
	return Transform{
		a: t.a * other.a + t.c * other.b
		b: t.b * other.a + t.d * other.b
		c: t.a * other.c + t.c * other.d
		d: t.b * other.c + t.d * other.d
		e: t.a * other.e + t.c * other.f + t.e
		f: t.b * other.e + t.d * other.f + t.f
	}
}

// apply transforms a point.
pub fn (t Transform) apply(p Vec2) Vec2 {
	x := f64(p.x)
	y := f64(p.y)
	return Vec2{
		x: f32(t.a * x + t.c * y + t.e)
		y: f32(t.b * x + t.d * y + t.f)
	}
}

// apply_f64 transforms a double-precision point.
pub fn (t Transform) apply_f64(x f64, y f64) Vec2 {
	return Vec2{
		x: f32(t.a * x + t.c * y + t.e)
		y: f32(t.b * x + t.d * y + t.f)
	}
}

// apply_rect transforms a rectangle and returns its axis-aligned bounds.
pub fn (t Transform) apply_rect(r Rect) Rect {
	pts := [
		t.apply(Vec2{ x: r.x, y: r.y })
		t.apply(Vec2{ x: r.x + r.w, y: r.y })
		t.apply(Vec2{ x: r.x, y: r.y + r.h })
		t.apply(Vec2{ x: r.x + r.w, y: r.y + r.h }),
	]
	mut x0 := pts[0].x
	mut y0 := pts[0].y
	mut x1 := x0
	mut y1 := y0
	for p in pts {
		x0 = math.min(x0, p.x)
		y0 = math.min(y0, p.y)
		x1 = math.max(x1, p.x)
		y1 = math.max(y1, p.y)
	}
	return Rect{ x: x0, y: y0, w: x1 - x0, h: y1 - y0 }
}

// determinant returns the matrix determinant.
pub fn (t Transform) determinant() f64 {
	return t.a * t.d - t.b * t.c
}

// inverse returns the inverse transform, or none for a singular matrix.
pub fn (t Transform) inverse() ?Transform {
	det := t.determinant()
	if math.abs(det) < 1e-12 {
		return none
	}
	return Transform{
		a: t.d / det
		b: -t.b / det
		c: -t.c / det
		d: t.a / det
		e: (t.c * t.f - t.d * t.e) / det
		f: (t.b * t.e - t.a * t.f) / det
	}
}

// is_identity reports whether the transform has no effect.
pub fn (t Transform) is_identity() bool {
	return t.a == 1 && t.b == 0 && t.c == 0 && t.d == 1 && t.e == 0 && t.f == 0
}

// str serializes the transform as an SVG transform list.
pub fn (t Transform) str() string {
	if t.is_identity() {
		return ''
	}
	return 'matrix(${fmt_num(t.a)},${fmt_num(t.b)},${fmt_num(t.c)},${fmt_num(t.d)},${fmt_num(t.e)},${fmt_num(t.f)})'
}

// parse_transform parses an SVG transform list (matrix/translate/scale/rotate/skewX/skewY).
pub fn parse_transform(input string) !Transform {
	mut s := input.trim_space()
	mut result := Transform.identity()
	for s.len > 0 {
		start := skip_ws(s, 0)
		s = s[start..]
		if s.len == 0 {
			break
		}
		mut name_end := 0
		for name_end < s.len && ((s[name_end] >= `a` && s[name_end] <= `z`) || (s[name_end] >= `A` && s[name_end] <= `Z`)) {
			name_end++
		}
		if name_end == 0 {
			return error('expected transform function: ${input}')
		}
		name := s[0..name_end]
		mut p := skip_ws(s, name_end)
		if p >= s.len || s[p] != `(` {
			return error('expected ( after ${name}')
		}
		p++
		mut args := []f64{}
		for {
			p = skip_ws(s, p)
			if p >= s.len {
				return error('unterminated ${name}()')
			}
			if s[p] == `)` {
				p++
				break
			}
			n := scan_number(s, p) or { return error('bad number in ${name}()') }
			args << n.value
			p += n.consumed
			p = skip_ws(s, p)
			if p < s.len && s[p] == `,` {
				p++
			}
		}
		part := transform_from_args(name, args) or { return err }
		result = result.then(part)
		s = s[p..]
	}
	return result
}

fn transform_from_args(name string, args []f64) !Transform {
	match name {
		'matrix' {
			if args.len != 6 {
				return error('matrix() needs 6 args')
			}
			return Transform.from_matrix(args[0], args[1], args[2], args[3], args[4], args[5])
		}
		'translate' {
			if args.len < 1 || args.len > 2 {
				return error('translate() needs 1 or 2 args')
			}
			y := if args.len == 2 { args[1] } else { 0 }
			return Transform.translate(args[0], y)
		}
		'scale' {
			if args.len < 1 || args.len > 2 {
				return error('scale() needs 1 or 2 args')
			}
			y := if args.len == 2 { args[1] } else { args[0] }
			return Transform.scale(args[0], y)
		}
		'rotate' {
			if args.len == 1 {
				return Transform.rotate(args[0])
			}
			if args.len == 3 {
				t := Transform.translate(args[1], args[2])
				return t.then(Transform.rotate(args[0])).then(Transform.translate(-args[1], -args[2]))
			}
			return error('rotate() needs 1 or 3 args')
		}
		'skewX' {
			if args.len != 1 {
				return error('skewX() needs 1 arg')
			}
			return Transform.shear(args[0], 0)
		}
		'skewY' {
			if args.len != 1 {
				return error('skewY() needs 1 arg')
			}
			return Transform.shear(0, args[0])
		}
		else {
			return error('unknown transform: ${name}')
		}
	}
}
