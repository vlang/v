@[has_globals]
module registry

struct Record {
	value int
}

__global registry = Record{ value: 7 }

pub const value = 42
pub const copied = registry.value

pub fn global_value() int {
	return registry.value
}
