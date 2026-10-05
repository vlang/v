module config

pub struct Cfg {
pub mut:
	name string
	n    int
}

// canonical_method identifies the canonical config type for reflection tests.
pub fn (c Cfg) canonical_method() {}
