module alias_chain_style

// used by vlib/v/tests/aliases/alias_chain_method_call_test.v

pub struct LayoutCache {
pub mut:
	direction int
}

pub struct Properties {
pub mut:
	width int
	cache &LayoutCache = unsafe { nil }
}

pub type Values = Properties

pub type MutableValues = Values

@[unsafe]
pub fn (mut p Properties) ensure_cache() &LayoutCache {
	if p.cache == unsafe { nil } {
		p.cache = &LayoutCache{}
	}
	return p.cache
}

pub fn (p &Properties) width_px() int {
	return p.width
}

pub fn (p Properties) doubled_width() int {
	return p.width * 2
}

pub fn (v Values) padded_width() int {
	return v.width + 100
}

pub fn (mut v Values) grow() {
	v.width++
}
