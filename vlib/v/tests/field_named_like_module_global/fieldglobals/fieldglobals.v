@[has_globals]
module fieldglobals

struct State {
mut:
	last int = 42
}

__global g State

// next advances the module global `g`.
pub fn next() int {
	g.last++
	return g.last
}
