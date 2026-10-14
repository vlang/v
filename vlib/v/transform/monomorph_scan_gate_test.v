module transform

import v.flat
import v.types

fn test_combined_generic_call_gate_preserves_the_previous_predicates() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.generic_fn_call_names['generic'] = true
	for kind in [flat.NodeKind.ident, .selector, .int_literal] {
		for name in ['', 'ordinary', 'generic', 'generic[int]', 'generic_T_int'] {
			base := a.add_node(flat.Node{ kind: kind, value: name })
			for indexed in [false, true] {
				mut callee := base
				if indexed {
					start := a.begin_children()
					a.add_child(base)
					callee = a.add_node(flat.Node{ kind: .index, children_start: start, children_count: 1 })
				}
				start := a.begin_children()
				a.add_child(callee)
				call := flat.Node{ kind: .call, children_start: start, children_count: 1 }
				assert t.call_may_target_generic(call) == (t.call_name_can_target_generic(call)
					|| t.synthetic_generic_call_has_exact_identity(call))
			}
		}
	}
	assert !t.call_may_target_generic(flat.Node{ kind: .call })
}
