@[has_globals]
module foreign

struct State {
mut:
	marker int
}

__global sources = State{ marker: 41 }

// marker reads the module-owned global after unrelated local operations.
pub fn marker() int { return sources.marker }
