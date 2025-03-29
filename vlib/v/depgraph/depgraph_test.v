module depgraph

fn test_resolve_has_deps() {
	mut g := new_dep_graph()
	g.add('X', ['Y']) // `X` is depend on `Y`
	g.add('A', [])
	g.add('B', [])
	g.add('C', [])
	g.add('D', [])
	resolved_graph := g.resolve()
	assert resolved_graph.nodes.len == 5
	assert resolved_graph.nodes[0].name == 'A'
	assert resolved_graph.nodes[1].name == 'B'
	assert resolved_graph.nodes[2].name == 'C'
	assert resolved_graph.nodes[3].name == 'D'
	assert resolved_graph.nodes[4].name == 'X'
	assert resolved_graph.nodes[4].deps[0] == 'Y'
}

fn test_resolve_no_deps() {
	mut g := new_dep_graph()
	g.add('X', [])
	g.add('A', [])
	g.add('B', [])
	g.add('C', [])
	g.add('D', [])
	resolved_graph := g.resolve()
	assert resolved_graph.nodes.len == 5
	assert resolved_graph.nodes[0].name == 'X'
	assert resolved_graph.nodes[1].name == 'A'
	assert resolved_graph.nodes[2].name == 'B'
	assert resolved_graph.nodes[3].name == 'C'
	assert resolved_graph.nodes[4].name == 'D'
}
