module shadowed_enum_mod

pub enum Kind {
	first
}

// str returns a multi-line name, which a struct auto-str must not re-indent.
pub fn (kind Kind) str() string {
	return 'first\nsecond'
}

pub struct Wrap {
pub:
	kind Kind
}
