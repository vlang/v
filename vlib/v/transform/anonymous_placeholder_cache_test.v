module transform

import v.flat
import v.types

fn test_anonymous_placeholder_queries_keep_exact_ids_and_ignore_module_context() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.skip_generics = true
	t.generic_unresolved_cache = &GenericUnresolvedCache{}
	for module_name in ['one', 'two', 'one'] {
		t.cur_module = module_name
		for text in ['struct', 'main.struct', '[]&?main.struct', 'map[main.struct]string'] {
			assert t.generic_arg_is_unresolved(flat.source_type_id(text)), text
		}
		for text in ['int', 'main.Struct', 'main.struct_value', '[]&main.Struct',
			'map[string]main.Struct'] {
			assert !t.generic_arg_is_unresolved(flat.source_type_id(text)), text
		}
	}
	placeholder := flat.source_type_id('collision.struct')
	mut collision := u32(0)
	for i in 0 .. 4096 {
		id := flat.source_type_id('CollisionType${i}')
		if id & 1023 == placeholder & 1023 {
			collision = id
			break
		}
	}
	assert collision != 0 && collision != placeholder
	for _ in 0 .. 3 {
		assert t.generic_arg_is_unresolved(placeholder)
		assert !t.generic_arg_is_unresolved(collision)
	}
}

fn test_alias_method_negative_index_does_not_hide_a_new_declaration() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('Alias', flat.source_type_int)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.collect_alias_methods()
	assert t.resolve_alias_receiver_method(flat.source_type_id('Alias'), 'touch') == none
	tc.fn_param_types['Alias.touch'] = [types.Type(types.int_)]
	tc.fn_ret_types['Alias.touch'] = types.Type(types.void_)
	assert (t.resolve_alias_receiver_method(flat.source_type_id('Alias'), 'touch') or {
		panic('missing alias method')
	}) == 'Alias.touch'
}
