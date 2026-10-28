module transform

import v.flat
import v.types

struct TransparentEscapeState {
mut:
	amp_ptrs         map[string]bool
	amp_sources      map[string][]string
	ptr_aliases      map[string]string
	method_receivers map[string]string
	capture_aliases  map[string][]string
	interface_boxes  map[string]bool
	returned         map[string]bool
	local_names      map[string]bool
	local_added      []string
	backing_sources  map[string][]string
}

fn transparent_escape_node(mut a flat.FlatAst, node flat.Node, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		...node
		children_start: start
		children_count: children.len
	})
}

fn (mut state TransparentEscapeState) scan(mut t Transformer, id flat.NodeId, clear bool) {
	t.scan_escape_pass(id, mut state.amp_ptrs, mut state.amp_sources, mut state.ptr_aliases,
		mut state.method_receivers, mut state.capture_aliases, mut state.interface_boxes,
		mut state.returned, mut state.local_names, mut state.local_added,
		mut state.backing_sources, clear)
}

fn test_transparent_escape_nodes_preserve_child_order_and_state() {
	for clear in [false, true] {
		for kind in [flat.NodeKind.expr_stmt, .paren, .cast_expr, .as_expr, .field_init, .struct_init,
			.array_literal, .array_init, .map_init, .selector, .index, .prefix, .postfix, .range,
			.assoc, .assert_stmt, .defer_stmt, .lock_expr, .string_interp, .infix, .or_expr] {
			mut a := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&a)
			mut t := new_transformer(mut a, &tc, map[string]bool{})
			decl := t.make_decl_assign('alias', t.make_ident('first'))
			assign := t.make_assign(t.make_ident('alias'), t.make_ident('second'))
			ret := transparent_escape_node(mut a, flat.Node{ kind: .return_stmt }, [t.make_ident('alias')])
			nested_assign := t.make_assign(t.make_ident('alias'), t.make_ident('nested'))
			nested_fn := transparent_escape_node(mut a, flat.Node{ kind: .fn_literal }, [nested_assign])
			// Stateful descendants expose traversal order independently of wrapper syntax.
			sequence := transparent_escape_node(mut a, flat.Node{ kind: .infix, op: .plus }, [
				decl,
				assign,
				ret,
				nested_fn,
			])
			wrapper := transparent_escape_node(mut a, flat.Node{ kind: kind, op: .plus }, [sequence])
			mut state := TransparentEscapeState{}
			state.method_receivers['alias'] = 'receiver'
			state.capture_aliases['alias'] = ['captured']
			state.interface_boxes['alias'] = true
			state.scan(mut t, wrapper, clear)
			assert state.ptr_aliases['alias'] == 'second', '${kind}'
			assert state.local_names['alias'], '${kind}'
			assert state.local_added == ['alias'], '${kind}'
			assert state.returned['alias'], '${kind}'
			assert ('alias' in state.method_receivers) == !clear, '${kind}'
			assert ('alias' in state.capture_aliases) == !clear, '${kind}'
			assert ('alias' in state.interface_boxes) == !clear, '${kind}'
		}
	}
}

fn test_transparent_escape_scan_reloads_children_after_reflected_ast_growth() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	receiver := a.add_node(flat.Node{ kind: .param, op: .dot, value: 'self', typ: flat.source_type_id('App') })
	transparent_escape_node(mut a, flat.Node{ kind: .fn_decl, value: 'App.first', typ: flat.source_type_int }, [receiver])
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('App')] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'main'
	body_assign := t.make_assign(t.make_ident('alias'), t.make_ident('reflected'))
	body := t.make_block([body_assign])
	loop := transparent_escape_node(mut a, flat.Node{
		kind:  .comptime_for
		value: 'method|methods'
		typ:   flat.source_type_id('App')
	}, [body])
	later := t.make_decl_assign('after', t.make_ident('alias'))
	wrapper := transparent_escape_node(mut a, flat.Node{ kind: .array_literal }, [
		loop,
		later,
	])
	// Tight buffers force the reflected analysis clones to grow both arrays.
	a.nodes = []flat.Node{len: a.nodes.len, cap: a.nodes.len, init: a.nodes[index]}
	a.children = []flat.NodeId{len: a.children.len, cap: a.children.len, init: a.children[index]}
	nodes_before := a.nodes.clone()
	children_before := a.children.clone()
	nodes_cap := a.nodes.cap
	children_cap := a.children.cap
	mut state := TransparentEscapeState{}
	state.scan(mut t, wrapper, true)
	assert state.ptr_aliases['alias'] == 'reflected'
	assert state.ptr_aliases['after'] == 'alias'
	assert state.local_names['after']
	assert a.nodes.cap > nodes_cap
	assert a.children.cap > children_cap
	assert a.nodes == nodes_before
	assert a.children == children_before
}
