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

fn test_alias_cache_does_not_admit_scope_dependent_witnesses() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.alias_cache = &AliasCache{
		texts: t.var_type_texts
	}
	value := a.add_node(flat.Node{ kind: .ident, value: 'value' })
	annotation_id := flat.source_type_id('ui2.Signal[typeof(__expr_${int(value)})]')
	slot := int(annotation_id & 1023)
	for payload in ['int', 'f64', '[]ui2.RowData'] {
		t.set_var_type('value', flat.source_type_id(payload))
		// A direct helper miss preserves the unresolved spelling without admitting
		// it to the cache used ahead of lexical witness resolution.
		assert t.normalize_type_alias_cached(annotation_id) == annotation_id
		assert annotation_id !in t.alias_cache.entries
		assert t.alias_cache.recent_generations[slot] != t.alias_cache.recent_generation
			|| t.alias_cache.recent_types[slot] != annotation_id
		assert t.alias_cache.canonical_types[slot] != annotation_id
		assert t.normalize_type_alias(annotation_id) == flat.source_type_id('ui2.Signal[${payload}]')
		assert a.node(value).typ == flat.source_type_id(payload)
		assert annotation_id !in t.alias_cache.entries
		assert t.alias_cache.recent_generations[slot] != t.alias_cache.recent_generation
			|| t.alias_cache.recent_types[slot] != annotation_id
	}
}
