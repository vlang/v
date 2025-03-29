module depgraph

fn test_resolve_has_deps() {
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
	resolved_graph := g.resolve()
	assert resolved_graph.display() == ' * A
 * B
 * C
 * E
 * F
 * H
 * G -> A
 * X -> Y
 * X -> G
 * D -> A
 * D -> X'
}

fn test_resolve_no_deps() {
	mut g := new_dep_graph()
	g.add('X', [])
	g.add('A', [])
	g.add('B', [])
	g.add('C', [])
	g.add('D', [])
	resolved_graph := g.resolve()
	assert resolved_graph.display() == ' * X
 * A
 * B
 * C
 * D'
}
