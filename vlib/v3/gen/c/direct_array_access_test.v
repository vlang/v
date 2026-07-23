module c

import v3.flat
import v3.token
import v3.types

fn direct_array_access_test_gen() &FlatGen {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	return &g
}

fn direct_array_access_test_attribute(mut a flat.FlatAst, target flat.NodeId) {
	mut directive := flat.Node{
		kind:  .directive
		value: '@attributes:${int(target)}'
	}
	directive.set_generic_params(['direct_array_access'])
	a.add_node(directive)
}

fn test_direct_array_access_attribute_matches_cloned_fn_source_position() {
	mut g := direct_array_access_test_gen()
	source_pos := token.new_span(1, 40, 80)
	original_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'at'
		pos:   source_pos
	})
	clone_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'at_T_int'
		pos:   source_pos
	})
	other_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'other'
		pos:   token.new_span(2, 40, 80)
	})
	direct_array_access_test_attribute(mut g.a, original_id)
	g.a.nodes[int(original_id)] = flat.Node{
		kind: .empty
		pos:  source_pos
	}

	attrs := g.direct_array_access_fns()
	assert attrs.contains(int(original_id), g.a.nodes[int(original_id)])
	assert attrs.contains(int(clone_id), g.a.nodes[int(clone_id)])
	assert !attrs.contains(int(other_id), g.a.nodes[int(other_id)])
}

fn test_direct_array_access_attribute_does_not_match_invalid_clone_position() {
	mut g := direct_array_access_test_gen()
	original_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'synthetic_original'
	})
	clone_id := g.a.add_node(flat.Node{
		kind:  .fn_decl
		value: 'unrelated_synthetic_clone'
	})
	direct_array_access_test_attribute(mut g.a, original_id)
	g.a.nodes[int(original_id)] = flat.Node{
		kind: .empty
	}

	attrs := g.direct_array_access_fns()
	assert attrs.contains(int(original_id), g.a.nodes[int(original_id)])
	assert !attrs.contains(int(clone_id), g.a.nodes[int(clone_id)])
}
