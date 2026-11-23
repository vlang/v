module svg

import math

// PaintKind distinguishes the three SVG paint servers.
pub enum PaintKind {
	none
	color
	gradient
	pattern
}

// Paint is a resolved fill or stroke value.
pub struct Paint {
pub:
	kind   PaintKind = .none
	color  Color
	ref_id string
}

// paint_from parses a paint value: none, a color, or url(#id).
pub fn paint_from(value string) Paint {
	s := value.trim_space()
	if s == '' || s == 'none' {
		return Paint{ kind: .none }
	}
	if s.starts_with('url(') && s.ends_with(')') {
		inner := s[4..s.len - 1].trim_space()
		if inner.starts_with('#') {
			return Paint{ kind: .gradient, ref_id: inner[1..] }
		}
		return Paint{ kind: .pattern, ref_id: inner }
	}
	c := parse_color(s) or { return Paint{ kind: .color, color: rgb(0, 0, 0) } }
	return Paint{ kind: .color, color: c }
}

// GradientKind is linear or radial.
pub enum GradientKind {
	linear
	radial
}

// Units selects objectBoundingBox or userSpaceOnUse coordinates.
pub enum Units {
	object_bbox
	user_space
}

// Spread is the gradient spread method beyond its endpoints.
pub enum Spread {
	pad
	reflect
	repeat
}

// Stop is one color stop in a gradient.
pub struct Stop {
pub:
	offset f64
	color  Color
}

// Gradient is a parsed linear or radial gradient.
pub struct Gradient {
pub mut:
	id        string
	kind      GradientKind = .linear
	x1        f64          = 0
	y1        f64          = 0
	x2        f64          = 1
	y2        f64          = 0
	cx        f64          = 0.5
	cy        f64          = 0.5
	r         f64          = 0.5
	fx        f64          = 0.5
	fy        f64          = 0.5
	units     Units        = .object_bbox
	spread    Spread       = .pad
	transform Transform
	stops     []Stop
}

fn attr_f(e &Element, name string, fallback f64) f64 {
	v := e.get_attr(name)
	if v == '' {
		return fallback
	}
	return parse_number(v) or { return fallback }
}

fn attr_units(e &Element, name string) Units {
	if e.get_attr(name) == 'userSpaceOnUse' {
		return .user_space
	}
	return .object_bbox
}

// gradient_from_element builds a Gradient from a <linearGradient> or
// <radialGradient> element, including inherited stops via xlink:href.
pub fn gradient_from_element(e &Element) !Gradient {
	kind := if e.tag == 'radialGradient' { GradientKind.radial } else { GradientKind.linear }
	mut g := Gradient{
		id:    e.id()
		kind:  kind
		units: attr_units(e, 'gradientUnits')
	}
	if e.has_attr('spreadMethod') {
		match e.get_attr('spreadMethod') {
			'reflect' {
				g.spread = .reflect
			}
			'repeat' {
				g.spread = .repeat
			}
			else {}
		}
	}
	if e.has_attr('gradientTransform') {
		g.transform = parse_transform(e.get_attr('gradientTransform')) or { Transform.identity() }
	}
	g.x1 = attr_f(e, 'x1', 0)
	g.y1 = attr_f(e, 'y1', 0)
	g.x2 = attr_f(e, 'x2', 1)
	g.y2 = attr_f(e, 'y2', 0)
	g.cx = attr_f(e, 'cx', 0.5)
	g.cy = attr_f(e, 'cy', 0.5)
	g.r = attr_f(e, 'r', 0.5)
	g.fx = attr_f(e, 'fx', g.cx)
	g.fy = attr_f(e, 'fy', g.cy)
	for child in e.children {
		if child.tag != 'stop' {
			continue
		}
		offset := attr_f(&child, 'offset', 0)
		st := cascade_style(&child, [])
		mut col := st.color('stop-color', rgb(0, 0, 0))
		mut op := st.number('stop-opacity', 1)
		if child.has_attr('stop-color') {
			col = parse_color(child.get_attr('stop-color')) or { col }
		}
		if child.has_attr('stop-opacity') {
			op = parse_number(child.get_attr('stop-opacity')) or { op }
		}
		g.stops << Stop{ offset: clamp_limits(offset, 0, 1), color: col.multiplied_alpha(op) }
	}
	return g
}

// Pattern is a parsed <pattern> element.
pub struct Pattern {
pub:
	id            string
	x             f64
	y             f64
	w             f64
	h             f64
	units         Units = .object_bbox
	content_units Units = .user_space
	transform     Transform
	children      []Element
}

// pattern_from_element builds a Pattern from a <pattern> element.
pub fn pattern_from_element(e &Element) !Pattern {
	return Pattern{
		id:            e.id()
		x:             attr_f(e, 'x', 0)
		y:             attr_f(e, 'y', 0)
		w:             attr_f(e, 'width', 0)
		h:             attr_f(e, 'height', 0)
		units:         attr_units(e, 'patternUnits')
		content_units: attr_units(e, 'patternContentUnits')
		transform:     if e.has_attr('patternTransform') {
			parse_transform(e.get_attr('patternTransform')) or { Transform.identity() }
		} else {
			Transform.identity()
		}
		children:      e.children
	}
}

// gradient_color samples a gradient at parameter t (0..1) with spread applied.
pub fn gradient_color(g Gradient, t f64) Color {
	if g.stops.len == 0 {
		return Color{ a: 0 }
	}
	if g.stops.len == 1 {
		return g.stops[0].color
	}
	mut u := t
	match g.spread {
		.pad {
			u = clamp_limits(u, 0, 1)
		}
		.repeat {
			u = u - math.floor(u)
		}
		.reflect {
			period := u - math.floor(u)
			flip := int(math.floor(u)) % 2 == 0
			u = if flip { period } else { 1 - period }
		}
	}
	for i in 1 .. g.stops.len {
		a := g.stops[i - 1]
		b := g.stops[i]
		if u <= b.offset {
			span := b.offset - a.offset
			f := if span < 1e-9 { 0.0 } else { (u - a.offset) / span }
			return lerp_color(a.color, b.color, clamp_limits(f, 0, 1))
		}
	}
	return g.stops[g.stops.len - 1].color
}

fn lerp_color(a Color, b Color, t f64) Color {
	return Color{
		r: u8(math.round(f64(a.r) + (f64(b.r) - f64(a.r)) * t))
		g: u8(math.round(f64(a.g) + (f64(b.g) - f64(a.g)) * t))
		b: u8(math.round(f64(a.b) + (f64(b.b) - f64(a.b)) * t))
		a: u8(math.round(f64(a.a) + (f64(b.a) - f64(a.a)) * t))
	}
}
