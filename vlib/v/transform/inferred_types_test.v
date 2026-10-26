module transform

import v.flat
import v.types

fn test_inferred_witness_uses_current_lexical_binding_for_each_specialization() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	value := a.add_node(flat.Node{ kind: .ident, value: 'value' })
	annotation := 'ui2.Signal[typeof(__expr_${int(value)})]'
	t.scope_parallel_workers = true
	t.scoped_base_nodes = a.nodes.len
	for payload in ['int', 'f64', '[]ui2.RowData'] {
		t.set_var_type('value', flat.source_type_id(payload))
		assert t.resolve_inferred_type_text(annotation) == 'ui2.Signal[${payload}]'
		assert a.node(value).typ == flat.source_type_id(payload)
		assert value in t.scoped_owned_base_log
	}
	// Resolving a transform-owned annotation must not publish a scratch-arena
	// Type into the original parser expression's semantic cache.
	if _ := tc.expr_type(value) {
		assert false, 'source witness acquired a synthesized checker type'
	}
}
