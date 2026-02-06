module depgraph

fn test_resolve_has_deps_no_cyclic() {
	mut g := new_dep_graph()
	g.add('A', [])
	g.add('B', [])
	g.add('X', ['Y', 'G'])
	g.add('C', [])
	g.add('D', ['A', 'X'])
	g.add('E', [])
	g.add('F', [])
	g.add('G', ['A'])
	g.add('H', [])
	g.add('Y', [])
	resolved_graph := g.resolve()
	assert resolved_graph.acyclic
	assert resolved_graph.display() == ' * A
 * B
 * C
 * E
 * F
 * H
 * Y
 * G -> A
 * X -> Y
 * X -> G
 * D -> A
 * D -> X'
}

fn test_resolve_has_deps_cyclic() {
	mut g := new_dep_graph()
	g.add('A', [])
	g.add('B', [])
	g.add('X', ['Y', 'G'])
	g.add('C', [])
	g.add('D', ['A', 'X'])
	g.add('E', [])
	g.add('F', [])
	g.add('G', ['D'])
	g.add('H', [])
	g.add('Y', [])
	resolved_graph := g.resolve()
	assert !resolved_graph.acyclic
	assert resolved_graph.display() == ' * A
 * B
 * C
 * E
 * F
 * H
 * Y
 * X -> Y
 * X -> G
 * D -> A
 * D -> X
 * G -> D'
}

fn test_resolve_no_deps() {
	mut g := new_dep_graph()
	g.add('X', [])
	g.add('A', [])
	g.add('B', [])
	g.add('C', [])
	g.add('D', [])
	resolved_graph := g.resolve()
	assert resolved_graph.acyclic
	assert resolved_graph.display() == ' * X
 * A
 * B
 * C
 * D'
}
