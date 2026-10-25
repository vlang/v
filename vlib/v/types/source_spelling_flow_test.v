module types

import v.flat

fn test_shared_field_owner_ids_preserve_exact_and_fallback_lookups() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'owner'
	tc.struct_shared_fields[flat.source_type_id('owner.Exact[int]')] = {
		'value': true
	}
	tc.struct_shared_fields[flat.source_type_id('owner.Generic')] = {
		'value': true
	}
	tc.struct_shared_fields[flat.source_type_id('Short')] = {
		'value': true
	}
	tc.struct_shared_fields[flat.source_type_id('owner.Local')] = {
		'value': true
	}
	tc.struct_shared_element_fields = tc.struct_shared_fields.clone()
	for spelling in ['owner.Exact[int]', 'owner.Generic[string]', 'other.Short', 'Local'] {
		id := flat.source_type_id(spelling)
		assert tc.struct_field_is_shared(id, 'value')
		assert tc.struct_field_has_shared_elements(id, 'value')
		assert !tc.struct_field_is_shared(id, 'missing')
		assert !tc.struct_field_has_shared_elements(id, 'missing')
	}
	assert !tc.struct_field_is_shared(0, 'value')
	assert !tc.struct_field_has_shared_elements(0, 'value')
	assert !tc.struct_field_is_shared(flat.source_type_id('owner.Other'), 'value')
	assert !tc.struct_field_has_shared_elements(flat.source_type_id('owner.Other'), 'value')
}

fn test_pointer_builtin_receiver_ids_preserve_aliases_and_pointees() {
	for name_id in [flat.source_type_charptr, flat.source_type_byteptr, flat.source_type_voidptr] {
		alias := Type(Alias{
			name_id:   name_id
			base_type: &builtin_int_type
		})
		assert pointer_builtin_receiver_type_id(alias) == name_id
	}
	byte_alias := Type(Alias{
		name_id:   flat.source_type_id('UserByte')
		base_type: &Type(u8_)
	})
	assert pointer_builtin_receiver_type_id(byte_alias) == flat.source_type_byteptr
	assert pointer_builtin_receiver_type_id(Type(Pointer{ base_type: &byte_alias })) == flat.source_type_byteptr
	assert pointer_builtin_receiver_type_id(Type(Pointer{ base_type: &Type(Char{}) })) == flat.source_type_charptr
	assert pointer_builtin_receiver_type_id(Type(Pointer{ base_type: &builtin_void_type })) == flat.source_type_voidptr
	assert pointer_builtin_receiver_type_id(builtin_int_type) == 0
	assert pointer_builtin_receiver_type_id(Type(Pointer{ base_type: &builtin_int_type })) == 0
}

fn test_fn_parameter_normalization_preserves_numeric_id_boundaries() {
	plain := flat.source_type_id('Dog')
	assert normalize_fn_type_param_type(plain) == plain
	for spelling in ['mut Dog', 'mut it &Dog', ' it fn (int) string ', 'mut []Dog'] {
		assert flat.source_type_name(normalize_fn_type_param_type(flat.source_type_id(spelling))) == normalize_fn_type_param_text(spelling)
	}
}

fn test_option_parameter_source_ids_preserve_wrapper_predicate() {
	for spelling in ['?Dog', '&?Dog', '&&?Dog', 'mut ?Dog', ' mut &&?Dog '] {
		assert param_source_type_is_option(flat.source_type_id(spelling))
	}
	for spelling in ['', 'Dog', '&Dog', '!Dog', '& ?Dog', 'mut & ?Dog'] {
		assert !param_source_type_is_option(flat.source_type_id(spelling))
	}
}
