module modifiers

pub enum Modifier as u32 {
	none  = 0
	shift = 1
	ctrl  = 2
}

// has reports whether the modifier matches, including the zero value.
pub fn (m Modifier) has(value Modifier) bool { return u32(m) & u32(value) > 0 || m == value }

// matches calls the custom enum method within its declaring module.
pub fn matches(value Modifier) bool { return value.has(.none) }
