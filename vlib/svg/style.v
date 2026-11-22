module svg

import strings

// Prop is a single CSS property (name: value).
pub struct Prop {
pub:
	name  string
	value string
}

// Style is an ordered list of CSS properties.
pub struct Style {
pub mut:
	props []Prop
}

// parse_style parses a CSS declaration list such as "fill:red; stroke: black".
pub fn parse_style(input string) !Style {
	mut s := Style{}
	if input.trim_space() == '' {
		return s
	}
	for raw in input.split(';') {
		decl := raw.trim_space()
		if decl == '' {
			continue
		}
		idx := decl.index(':') or { return error('invalid style declaration: ${decl}') }
		name := decl[0..idx].trim_space().to_lower()
		value := decl[idx + 1..].trim_space()
		if name == '' || value == '' {
			return error('invalid style declaration: ${decl}')
		}
		s.set(name, value)
	}
	return s
}

// get returns the value of a property, or '' when absent.
pub fn (s Style) get(name string) string {
	for p in s.props {
		if p.name == name {
			return p.value
		}
	}
	return ''
}

// has reports whether a property is present.
pub fn (s Style) has(name string) bool {
	for p in s.props {
		if p.name == name {
			return true
		}
	}
	return false
}

// set replaces or appends a property.
pub fn (mut s Style) set(name string, value string) {
	for i, p in s.props {
		if p.name == name {
			s.props[i] = Prop{ name: name, value: value }
			return
		}
	}
	s.props << Prop{ name: name, value: value }
}

// remove deletes a property.
pub fn (mut s Style) remove(name string) {
	mut out := []Prop{cap: s.props.len}
	for p in s.props {
		if p.name != name {
			out << p
		}
	}
	s.props = out
}

// merge copies properties from other that are not already set.
pub fn (mut s Style) merge(other Style) {
	for p in other.props {
		if !s.has(p.name) {
			s.props << p
		}
	}
}

// is_empty reports whether the style has no properties.
pub fn (s Style) is_empty() bool {
	return s.props.len == 0
}

// len returns the number of properties.
pub fn (s Style) len() int {
	return s.props.len
}

// str serializes the style as a CSS declaration list.
pub fn (s Style) str() string {
	mut sb := strings.new_builder(s.props.len * 16)
	for i, p in s.props {
		if i > 0 {
			sb.write_string('; ')
		}
		sb.write_string(p.name)
		sb.write_string(': ')
		sb.write_string(p.value)
	}
	return sb.str()
}

// color resolves a property as a color, using fallback when absent or invalid.
pub fn (s Style) color(name string, fallback Color) Color {
	v := s.get(name)
	if v == '' {
		return fallback
	}
	return parse_color(v) or { return fallback }
}

// length resolves a property as a length in px.
pub fn (s Style) length(name string, font_size f64, viewport f64, fallback f64) f64 {
	v := s.get(name)
	if v == '' {
		return fallback
	}
	l := parse_length(v) or { return fallback }
	return l.to_px(96, font_size, viewport)
}

// number resolves a property as a plain number.
pub fn (s Style) number(name string, fallback f64) f64 {
	v := s.get(name)
	if v == '' {
		return fallback
	}
	return parse_number(v) or { return fallback }
}
