module datatypes

fn before(order []string, a string, b string) bool {
	return order.index(a) < order.index(b)
}

fn test_graph_diamond_order() {
	mut g := new_graph[string]()
	g.add_edge('compile', 'link')
	g.add_edge('compile', 'test')
	g.add_edge('link', 'package')
	g.add_edge('test', 'package')
	order := g.topo_sort()!
	assert order.len == 4
	assert order[0] == 'compile', 'sources first'
	assert order[3] == 'package', 'sinks last'
	assert before(order, 'compile', 'link')
	assert before(order, 'compile', 'test')
	assert before(order, 'link', 'package')
	assert before(order, 'test', 'package')
	assert !g.has_cycle()
	assert g.successors('compile').len == 2
	assert g.successors('package') == []
}

fn test_graph_cycle_errors() {
	mut g := new_graph[string]()
	g.add_edge('a', 'b')
	g.add_edge('b', 'c')
	g.add_edge('c', 'a')
	assert g.has_cycle()
	g.topo_sort() or {
		assert err.msg().contains('cycle'), 'error must name the problem'
		assert err.msg().contains('a') && err.msg().contains('b')
			&& err.msg().contains('c'), 'error must name the stuck nodes'
		return
	}
	assert false, 'cyclic graph must error'
}

fn test_graph_self_loop_and_isolated() {
	mut g := new_graph[string]()
	g.add_edge('x', 'x')
	assert g.has_cycle(), 'self-loop is a cycle'
	mut h := new_graph[string]()
	h.add_node('lonely')
	assert !h.has_cycle()
	assert h.topo_sort()! == ['lonely']
}

fn test_graph_int_keys_disconnected() {
	mut g := new_graph[int]()
	g.add_edge(1, 2)
	g.add_node(99)
	order := g.topo_sort()!
	assert order.len == 3
	assert before_int(order, 1, 2)
	assert order.contains(99)
}

fn before_int(order []int, a int, b int) bool {
	return order.index(a) < order.index(b)
}

fn test_successors_cannot_mutate_graph() {
	mut g := new_graph[string]()
	g.add_edge('a', 'b')
	mut next := g.successors('a')
	next[0] = 'unknown'
	next << 'another'
	assert g.successors('a') == ['b']
	assert g.topo_sort()! == ['a', 'b']
}

fn test_parallel_edges_and_empty_graph() {
	mut g := new_graph[int]()
	assert g.topo_sort()! == []
	g.add_edge(1, 2)
	g.add_edge(1, 2)
	assert g.topo_sort()! == [1, 2]
	assert !g.has_cycle()
}
