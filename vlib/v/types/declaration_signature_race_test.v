module types

import v.flat

fn signature_race_mark_variadic(mut a flat.FlatAst, call flat.NodeId, ready chan bool) {
	ready <- true
	for _ in 0 .. 10000000 {
		a.nodes[int(call)].flags |= flat.node_flag_variadic_call
	}
}

fn test_declaration_type_queries_do_not_read_concurrent_call_flags() {
	mut a := &flat.FlatAst{}
	call := a.add(.call)
	body_start := a.begin_children()
	a.add_child(call)
	body := a.add_node(flat.Node{
		kind:           .block
		children_start: body_start
		children_count: 1
	})
	start := a.begin_children()
	a.add_child(body)
	declaration := a.add_node(flat.Node{
		kind:           .fn_decl
		value:          'probe'
		typ:            flat.source_type_void
		children_start: start
		children_count: 1
	})
	assert a.child(a.node(declaration), 0) == body
	assert a.child(a.node(body), 0) == call
	mut tc := TypeChecker.new(a)
	assert voidptr(tc.a) == voidptr(a)
	ready := chan bool{cap: 1}
	writer := spawn signature_race_mark_variadic(mut a, call, ready)
	_ := <-ready
	for _ in 0 .. 10000 {
		tc.check_decl_type_strings(declaration, *a.node(declaration))
	}
	writer.wait()
	assert a.nodes[int(call)].flags & flat.node_flag_variadic_call != 0
	assert tc.errors.len == 0
}
