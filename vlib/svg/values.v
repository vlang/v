module svg

import math
import strings

// LengthUnit is the unit of an SVG <length>.
pub enum LengthUnit {
	none
	px
	em
	ex
	ch
	percent
	pt
	pc
	cm
	mm
	inches
	q
}

// Length is an SVG length: a value plus its unit.
pub struct Length {
pub:
	val  f64
	unit LengthUnit = .none
}

// parse_length parses a number with an optional unit suffix.
pub fn parse_length(input string) !Length {
	s := input.trim_space()
	if s.len == 0 {
		return error('empty length')
	}
	n := scan_number(s, 0) or { return error('invalid length: ${input}') }
	unit := s[n.consumed..].trim_space()
	mut u := LengthUnit.none
	if unit == 'px' {
		u = .px
	} else if unit == 'em' {
		u = .em
	} else if unit == 'ex' {
		u = .ex
	} else if unit == 'ch' {
		u = .ch
	} else if unit == '%' {
		u = .percent
	} else if unit == 'pt' {
		u = .pt
	} else if unit == 'pc' {
		u = .pc
	} else if unit == 'cm' {
		u = .cm
	} else if unit == 'mm' {
		u = .mm
	} else if unit == 'in' {
		u = .inches
	} else if unit == 'q' {
		u = .q
	} else if unit != '' {
		return error('unknown length unit: ${unit}')
	}
	return Length{ val: n.value, unit: u }
}

// to_px resolves the length to user units. font_size is the current font
// size in px, viewport is the relevant viewport dimension in px, and dpi
// controls physical unit conversion (96 is the CSS/SVG default).
pub fn (l Length) to_px(dpi f64, font_size f64, viewport f64) f64 {
	return match l.unit {
		.none, .px { l.val }
		.em { l.val * font_size }
		.ex, .ch { l.val * font_size * 0.5 }
		.percent { l.val / 100 * viewport }
		.inches { l.val * dpi }
		.cm { l.val * dpi / 2.54 }
		.mm { l.val * dpi / 25.4 }
		.q { l.val * dpi / 101.6 }
		.pt { l.val * dpi / 72 }
		.pc { l.val * dpi / 6 }
	}
}

// str serializes the length.
pub fn (l Length) str() string {
	unit := match l.unit {
		.none { '' }
		.px { 'px' }
		.em { 'em' }
		.ex { 'ex' }
		.ch { 'ch' }
		.percent { '%' }
		.pt { 'pt' }
		.pc { 'pc' }
		.cm { 'cm' }
		.mm { 'mm' }
		.inches { 'in' }
		.q { 'q' }
	}
	return '${fmt_num(l.val)}${unit}'
}

// AngleUnit is the unit of an SVG <angle>.
pub enum AngleUnit {
	deg
	rad
	grad
	turn
}

// Angle is an SVG angle.
pub struct Angle {
pub:
	val  f64
	unit AngleUnit = .deg
}

// parse_angle parses an angle such as 45, 45deg, 0.5turn or 1.57rad.
pub fn parse_angle(input string) !Angle {
	s := input.trim_space()
	n := scan_number(s, 0) or { return error('invalid angle: ${input}') }
	unit := s[n.consumed..].trim_space().to_lower()
	mut u := AngleUnit.deg
	if unit == 'rad' {
		u = .rad
	} else if unit == 'grad' {
		u = .grad
	} else if unit == 'turn' {
		u = .turn
	} else if unit != '' && unit != 'deg' {
		return error('unknown angle unit: ${unit}')
	}
	return Angle{ val: n.value, unit: u }
}

// to_deg converts the angle to degrees.
pub fn (a Angle) to_deg() f64 {
	return match a.unit {
		.deg { a.val }
		.rad { a.val * 180 / math.pi }
		.grad { a.val * 0.9 }
		.turn { a.val * 360 }
	}
}

// Align is the alignment part of preserveAspectRatio.
pub enum Align {
	xmin_ymin
	xmin_ymid
	xmin_ymax
	xmid_ymin
	xmid_ymid
	xmid_ymax
	xmax_ymin
	xmax_ymid
	xmax_ymax
	none
}

// AspectRatio is a parsed preserveAspectRatio value.
pub struct AspectRatio {
pub:
	align Align = .xmid_ymid
	slice bool
}

// parse_aspect_ratio parses a preserveAspectRatio attribute.
pub fn parse_aspect_ratio(input string) !AspectRatio {
	s := input.trim_space()
	if s == '' || s == 'none' {
		return AspectRatio{ align: .none }
	}
	parts := s.split(' ')
	mut align := Align.xmid_ymid
	if parts.len > 0 {
		match parts[0] {
			'xMinYMin' {
				align = .xmin_ymin
			}
			'xMinYMid' {
				align = .xmin_ymid
			}
			'xMinYMax' {
				align = .xmin_ymax
			}
			'xMidYMin' {
				align = .xmid_ymin
			}
			'xMidYMid' {
				align = .xmid_ymid
			}
			'xMidYMax' {
				align = .xmid_ymax
			}
			'xMaxYMin' {
				align = .xmax_ymin
			}
			'xMaxYMid' {
				align = .xmax_ymid
			}
			'xMaxYMax' {
				align = .xmax_ymax
			}
			else {
				return error('invalid preserveAspectRatio: ${input}')
			}
		}
	}
	mut slice := false
	if parts.len > 1 {
		if parts[1] == 'slice' {
			slice = true
		} else if parts[1] != 'meet' {
			return error('invalid preserveAspectRatio meet/slice: ${input}')
		}
	}
	return AspectRatio{ align: align, slice: slice }
}

// ViewBox is an SVG viewBox: min-x, min-y, width, height.
pub struct ViewBox {
pub:
	min_x f64
	min_y f64
	w     f64
	h     f64
}

// parse_viewbox parses four numbers separated by spaces/commas.
pub fn parse_viewbox(input string) !ViewBox {
	mut vals := []f64{}
	mut buf := strings.new_builder(input.len)
	for i, c in input {
		if c == `,` || c == ` ` || c == `\t` || c == `\n` || c == `\r` {
			if buf.len > 0 {
				vals << parse_number(buf.str()) or { return error('invalid viewBox: ${input}') }
				buf = strings.new_builder(input.len)
			}
		} else {
			buf.write_string(input[i..i + 1])
		}
	}
	if buf.len > 0 {
		vals << parse_number(buf.str()) or { return error('invalid viewBox: ${input}') }
	}
	if vals.len != 4 {
		return error('viewBox needs 4 numbers: ${input}')
	}
	return ViewBox{ min_x: vals[0], min_y: vals[1], w: vals[2], h: vals[3] }
}

// matrix_for computes the transform mapping this viewBox into viewport,
// honoring preserveAspectRatio alignment and meet/slice.
pub fn (vb ViewBox) matrix_for(viewport Rect, ar AspectRatio) Transform {
	if vb.w <= 0 || vb.h <= 0 || ar.align == .none {
		return Transform.translate(-vb.min_x, -vb.min_y)
	}
	sx := f64(viewport.w) / vb.w
	sy := f64(viewport.h) / vb.h
	s := if ar.slice { math.max(sx, sy) } else { math.min(sx, sy) }
	rendered_w := vb.w * s
	rendered_h := vb.h * s
	mut tx := f64(viewport.x) + (f64(viewport.w) - rendered_w) / 2
	mut ty := f64(viewport.y) + (f64(viewport.h) - rendered_h) / 2
	match ar.align {
		.xmin_ymin, .xmin_ymid, .xmin_ymax {
			tx = f64(viewport.x)
		}
		.xmax_ymin, .xmax_ymid, .xmax_ymax {
			tx = f64(viewport.x) + f64(viewport.w) - rendered_w
		}
		else {}
	}
	match ar.align {
		.xmin_ymin, .xmid_ymin, .xmax_ymin {
			ty = f64(viewport.y)
		}
		.xmin_ymax, .xmid_ymax, .xmax_ymax {
			ty = f64(viewport.y) + f64(viewport.h) - rendered_h
		}
		else {}
	}
	return Transform{
		a: s
		d: s
		e: tx - vb.min_x * s
		f: ty - vb.min_y * s
	}
}

// str serializes the viewBox.
pub fn (vb ViewBox) str() string {
	return '${fmt_num(vb.min_x)} ${fmt_num(vb.min_y)} ${fmt_num(vb.w)} ${fmt_num(vb.h)}'
}
