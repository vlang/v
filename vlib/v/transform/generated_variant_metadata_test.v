module transform

import v.flat

fn test_generated_variant_metadata_keeps_first_type_and_other_payload_values() {
	mut a := flat.FlatAst.new()
	variant_id := flat.source_type_id('pkg.Value[&pkg.Item]')
	fn_id := flat.source_type_id('fn (pkg.Value[&pkg.Item]) bool')
	selector := a.add_node(flat.Node{
		kind:    .selector
		value:   '_pkg__Value'
		typ:     flat.source_type_id('&pkg.Value[&pkg.Item]')
		payload: flat.node_payload_with_fn_value_type(['existing'], fn_id)
	})
	mut t := Transformer{
		a: &a
	}
	t.mark_generated_variant_access(selector, variant_id)
	assert t.generated_variant_access_type(selector) or { panic('missing generated variant') } == variant_id
	assert a.nodes[int(selector)].generic_params() == ['existing', generated_variant_access_marker]
	assert a.nodes[int(selector)].fn_value_type_id() == fn_id
	owned := a.nodes[int(selector)].clone_owned()
	assert owned.generated_variant_type_id() == variant_id
	assert owned.fn_value_type_id() == fn_id
	t.mark_generated_variant_access(selector, flat.source_type_id('pkg.Other'))
	assert t.generated_variant_access_type(selector) or { panic('missing generated variant') } == variant_id
	assert a.nodes[int(selector)].generic_params() == ['existing', generated_variant_access_marker]
}

fn test_generated_variant_metadata_survives_deferred_base_write() {
	mut a := flat.FlatAst.new()
	variant_id := flat.source_type_id('pkg.Variant')
	selector := a.add_val(.selector, '_pkg__Variant')
	mut t := Transformer{
		a:                    &a
		base_write_intercept: true
		shared_base_nodes:    a.nodes.len
		defer_oor_writes:     true
	}
	t.mark_generated_variant_access(selector, variant_id)
	assert a.nodes[int(selector)].generated_variant_type_id() == 0
	assert t.deferred_base_writes.len == 1
	t.flush_deferred_base_writes()
	assert t.generated_variant_access_type(selector) or { panic('missing deferred variant') } == variant_id
	assert a.nodes[int(selector)].generic_params() == [generated_variant_access_marker]
}
