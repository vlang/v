struct ReallocNode {
mut:
	attrs []int
}

struct ReallocArena {
mut:
	nodes []ReallocNode
}

fn (mut a ReallocArena) add_many() []int {
	mut out := []int{}
	for _ in 0 .. 3 {
		a.nodes << ReallocNode{}
		out << a.nodes.len - 1
	}
	return out
}

fn test_assign_index_field_rhs_realloc() {
	mut a := ReallocArena{}
	a.nodes << ReallocNode{}
	// The right-hand side appends to `a.nodes`, reallocating its buffer.
	// The address of `a.nodes[0]` must be taken after the call returns.
	a.nodes[0].attrs = a.add_many()
	assert a.nodes.len == 4
	assert a.nodes[0].attrs.len == 3
	assert a.nodes[0].attrs == [1, 2, 3]
}
