module datatypes

// Graph is a directed graph with topological sorting (Kahn's algorithm). It
// is the V-idiomatic equivalent of Python's `graphlib`: the core of build
// systems, DI containers and task schedulers. Node keys must be supported V map key types.
pub struct Graph[T] {
mut:
	nodes map[T]bool
	succ  map[T][]T
}

// new_graph creates an empty Graph.
pub fn new_graph[T]() Graph[T] {
	return Graph[T]{}
}

// add_node ensures v exists (isolated nodes need this; edge endpoints are
// added implicitly).
pub fn (mut g Graph[T]) add_node(v T) {
	if v !in g.nodes {
		g.nodes[v] = true
	}
}

// add_edge adds the directed edge from -> to (creating both endpoints).
// Parallel edges are tolerated.
pub fn (mut g Graph[T]) add_edge(from T, to T) {
	g.add_node(from)
	g.add_node(to)
	mut outs := g.succ[from]
	outs << to
	g.succ[from] = outs
}

// successors returns an independent array of direct successors of v (empty if none).
pub fn (g &Graph[T]) successors(v T) []T {
	if v !in g.succ {
		return []T{}
	}
	return g.succ[v].clone()
}

// has_cycle reports whether the graph contains a directed cycle
// (a self-loop counts).
pub fn (g &Graph[T]) has_cycle() bool {
	_, leftover := g.kahn()
	return leftover.len > 0
}

// topo_sort returns the nodes in dependency order (dependencies first).
// Cyclic graphs are an error naming nodes that could not be ordered,
// including successors blocked by a cycle.
pub fn (g &Graph[T]) topo_sort() ![]T {
	order, leftover := g.kahn()
	if leftover.len > 0 {
		return error('datatypes: graph has a cycle involving ${leftover}')
	}
	return order
}

// kahn runs Kahn's algorithm, returning the topological prefix and the nodes
// that could not be ordered (non-empty exactly for cyclic graphs).
fn (g &Graph[T]) kahn() ([]T, []T) {
	mut indeg := map[T]int{}
	for n in g.nodes.keys() {
		indeg[n] = 0
	}
	for _, tos in g.succ {
		for to in tos {
			indeg[to] = indeg[to] + 1
		}
	}
	mut queue := []T{}
	for n, d in indeg {
		if d == 0 {
			queue << n
		}
	}
	mut order := []T{}
	mut head := 0
	for head < queue.len {
		n := queue[head]
		head++
		order << n
		if n in g.succ {
			for to in g.succ[n] {
				indeg[to] = indeg[to] - 1
				if indeg[to] == 0 {
					queue << to
				}
			}
		}
	}
	mut leftover := []T{}
	for n, d in indeg {
		if d > 0 {
			leftover << n
		}
	}
	return order, leftover
}
