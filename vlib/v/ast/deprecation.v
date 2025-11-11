module ast

import v.token

pub struct Deprecation {
pub mut:
	deprecated           bool
	deprecated_pos       token.Pos
	deprecated_after     bool
	deprecated_after_pos token.Pos
}

pub fn (d &Deprecation) message() string {
	return '@[deprecated_after] is only valid, in the presence of a `@[deprecated]` attribute'
}

pub fn (d &Deprecation) pos() token.Pos {
	return d.deprecated_after_pos
}

pub fn Deprecation.check_invalid_attributes(attrs []Attr) ?Deprecation {
	mut d := Deprecation{}
	for attr in attrs {
		match attr.name {
			'deprecated' {
				d.deprecated = true
				d.deprecated_pos = attr.pos
			}
			'deprecated_after' {
				d.deprecated_after = true
				d.deprecated_after_pos = attr.pos
			}
			else {}
		}
	}
	if d.deprecated_after && !d.deprecated {
		return d
	}
	return none
}
