module ssa

import v.flat

// Open generic declarations have no concrete ABI. Only the monomorphized
// function nodes can be registered or lowered, as with the C backend.
fn (b &Builder) source_fn_is_open_generic_template(node flat.Node, node_index int, module_name string) bool {
	if node.generic_params().len > 0 {
		return true
	}
	if !node.value.contains('.') || b.a.specialized_fn_nodes[node_index] {
		return false
	}
	receiver := node.value.all_before_last('.')
	receiver_id := flat.source_type_id(receiver)
	shape := flat.source_type_shape(receiver_id)
	if !shape.has(.bracketed) || shape.has(.leading_bracket)
		|| shape.has(.function_compact) || shape.has(.function_spaced) {
		return false
	}
	syntax := flat.source_type_syntax(receiver_id)
	base := syntax.cgen_raw_app_base()
	args := syntax.cgen_raw_app_args()
	if !syntax.cgen_raw_app_valid() || args.len == 0 {
		return false
	}
	// A receiver declaration spells its own parameters in brackets. Check the
	// source text too, since declaration-only caches can omit the generic index.
	for arg in args {
		name := flat.source_type_name(arg).trim_space()
		if name.len == 1 && name[0] >= `A` && name[0] <= `Z` {
			return true
		}
	}
	if b.tc == unsafe { nil } {
		return false
	}
	mut candidates := [base]
	if !flat.source_type_shape(base).has(.dotted) && module_name !in ['', 'main', 'builtin'] {
		candidates << qualify_type_ref_name(base, module_name)
	}
	for candidate in candidates {
		params := b.tc.struct_generic_params[candidate] or { continue }
		for arg in args {
			if arg in params {
				return true
			}
		}
	}
	return false
}
