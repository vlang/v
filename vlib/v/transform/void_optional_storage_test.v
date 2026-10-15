module transform

import v.flat
import v.types

fn test_void_optional_storage_does_not_clone_a_payload() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('VoidResult', flat.source_type_id('!'))
	tc.type_aliases.set_id('VoidOption', flat.source_type_id('?'))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for typ in ['!', '?', '!void', '?void', '!!', '??', 'VoidResult', 'VoidOption'] {
		t.set_var_type('source', flat.source_type_id(typ))
		source := t.make_ident('source')
		assert t.clone_borrowed_storage_projection(source, source, flat.source_type_id(typ)) == source
		assert t.clone_owned_array_value_for_capture(source, flat.source_type_id(typ)) == source
		assert t.pending_stmts.len == 0
	}
}

fn test_void_optional_mut_parameter_has_no_array_storage() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.mut_param_values['source'] = true
	for typ in ['&!', '&?', '&!void', '&?void'] {
		t.set_var_type('source', flat.source_type_id(typ))
		assert !t.array_storage_source_is_mut_param(t.make_ident('source'))
	}
}
