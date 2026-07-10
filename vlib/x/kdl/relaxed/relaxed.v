module relaxed

// RelaxedNonCompliant controls parsing of non-standard KDL syntax.
// Uses bitmask flags matching kdl-go's relaxed.Flags design.
pub struct RelaxedNonCompliant {
pub mut:
	flags int
}

pub const nginx_syntax = 1
pub const yaml_toml_assignments = 1 << 1
pub const multiplier_suffixes = 1 << 2

pub fn new() RelaxedNonCompliant {
	return RelaxedNonCompliant{}
}

// permit indicates whether a given flag is set.
pub fn (r RelaxedNonCompliant) permit(flag int) bool {
	return (r.flags & flag) != 0
}
