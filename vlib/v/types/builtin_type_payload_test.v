module types

import v.flat

fn builtin_type_payloads_are_identical(first Type, second Type) bool {
	first_word, first_tail, _ := type_value_words(&first)
	second_word, second_tail, _ := type_value_words(&second)
	return first_word == second_word && first_tail == second_tail
}

fn test_builtin_literal_resolution_reuses_immutable_payloads() {
	mut a := flat.FlatAst.new()
	integer_nodes := [a.add_val(.int_literal, '1'), a.add_val(.int_literal, '200')]
	boolean_nodes := [a.add_val(.bool_literal, 'true'), a.add_val(.bool_literal, 'false')]
	string_nodes := [a.add_val(.string_literal, 'first'), a.add_val(.string_literal, 'second')]
	tc := TypeChecker.new(&a)
	for nodes in [integer_nodes, boolean_nodes, string_nodes] {
		// Bypass all expression/type memos: different nodes must still return the
		// same immutable builtin payload, without allocating a new sum-type box.
		first := tc.resolve_type_uncached(nodes[0])
		second := tc.resolve_type_uncached(nodes[1])
		assert first == second
		assert builtin_type_payloads_are_identical(first, second)
	}
	assert tc.resolve_type_uncached(integer_nodes[0]) == Type(int_)
	assert tc.resolve_type_uncached(boolean_nodes[0]) == Type(bool_)
	assert tc.resolve_type_uncached(string_nodes[0]) == Type(string_)
}

fn test_builtin_name_resolution_reuses_the_known_payloads() {
	names := ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'i128', 'u128',
		'f32', 'f64', 'string', 'char', 'rune', 'isize', 'uint', 'usize', 'void', 'voidptr', 'charptr',
		'byteptr', 'nil', 'none']
	expected := [builtin_bool_type, builtin_int_type, builtin_i8_type, builtin_i16_type,
		builtin_i32_type, builtin_i64_type, builtin_u8_type, builtin_u16_type, builtin_u32_type,
		builtin_u64_type, builtin_i128_type, builtin_u128_type, builtin_f32_type, builtin_f64_type,
		builtin_string_type, builtin_char_type, builtin_rune_type, builtin_isize_type,
		builtin_usize_type, builtin_usize_type, builtin_void_type, builtin_voidptr_type,
		builtin_charptr_type, builtin_byteptr_type, builtin_nil_type, builtin_none_type]
	for i, name in names {
		first := builtin_type_value(name)
		assert first == expected[i]
		assert builtin_type_payloads_are_identical(first, expected[i])
		assert builtin_type_payloads_are_identical(first, builtin_type_value(name))
	}
	array := builtin_type_value('array')
	assert array is Array
	assert array.elem_type is Void
	assert builtin_type_value('missing') is Unknown
}

fn test_builtin_method_signatures_reuse_immutable_payloads() {
	mut a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	for _ in 0 .. 3 {
		info := tc.pointer_builtin_method_call_info(builtin_charptr_type, 'vstring_with_len') or {
			assert false
			return
		}
		assert info.params.len == 2
		assert builtin_type_payloads_are_identical(info.params[0], builtin_charptr_type)
		assert builtin_type_payloads_are_identical(info.params[1], builtin_int_type)
		assert builtin_type_payloads_are_identical(info.return_type, builtin_string_type)
		hex_info := tc.builtin_receiver_method_call_info(builtin_int_type, 'hex') or {
			assert false
			return
		}
		assert builtin_type_payloads_are_identical(hex_info.return_type, builtin_string_type)
	}
}

fn test_cached_generic_method_type_keeps_its_original_payload() {
	mut a := flat.FlatAst.new()
	receiver := a.add_val(.ident, 'holder')
	selector_start := a.begin_children()
	a.add_child(receiver)
	selector := a.add_node(flat.Node{
		kind:           .selector
		value:          'first'
		children_start: selector_start
		children_count: 1
	})
	argument := a.add_val(.ident, 'int')
	index_start := a.begin_children()
	a.add_child(selector)
	a.add_child(argument)
	method := a.add_node(flat.Node{
		kind:           .index
		children_start: index_start
		children_count: 2
	})
	mut tc := TypeChecker.new(&a)
	method_type := Type(FnType{
		return_type: builtin_int_type
	})
	tc.remember_expr_type(method, method_type)
	for _ in 0 .. 3 {
		resolved := tc.resolve_type_uncached(method)
		assert resolved == method_type
		assert builtin_type_payloads_are_identical(resolved, method_type)
	}
}

fn test_forwarded_types_keep_signature_and_numeric_compatibility() {
	a := flat.FlatAst.new()
	tc := TypeChecker.new(&a)
	aliased_integer := Type(Alias{
		name:      'Measure'
		base_type: builtin_int_type
	})
	actual := Type(FnType{
		params:      [aliased_integer]
		return_type: builtin_int_type
	})
	expected := Type(FnType{
		params:      [builtin_int_type]
		return_type: builtin_int_type
	})
	wrong_parameter := Type(FnType{
		params:      [builtin_bool_type]
		return_type: builtin_int_type
	})
	mut_parameter := Type(FnType{
		params:      [builtin_int_type]
		params_mut:  [true]
		return_type: builtin_int_type
	})
	assert actual.name() != expected.name()
	assert tc.type_compatible(actual, expected)
	assert !tc.type_compatible(actual, wrong_parameter)
	assert !tc.type_compatible(actual, mut_parameter)
	assert tc.type_compatible(aliased_integer, builtin_i64_type)
	assert tc.type_compatible(builtin_rune_type, builtin_int_type)
	assert tc.type_compatible(builtin_u8_type, builtin_rune_type)
	assert !tc.type_compatible(builtin_bool_type, builtin_int_type)
}

fn test_builtin_literal_payloads_survive_disposable_worker_scopes() {
	$if prealloc {
		mut a := flat.FlatAst.new()
		integer := a.add_val(.int_literal, '1')
		boolean := a.add_val(.bool_literal, 'true')
		text := a.add_val(.string_literal, 'retained')
		tc := TypeChecker.new(&a)
		mut integer_type := builtin_void_type
		mut boolean_type := builtin_void_type
		mut string_type := builtin_void_type
		scope := unsafe { prealloc_scope_begin() }
		worker := tc.fork_for_parallel_check()
		integer_type = worker.resolve_type_uncached(integer)
		boolean_type = worker.resolve_type_uncached(boolean)
		string_type = worker.resolve_type_uncached(text)
		unsafe {
			prealloc_scope_leave(scope)
			prealloc_scope_free_after(scope)
		}
		assert integer_type == Type(int_)
		assert boolean_type == Type(bool_)
		assert string_type.name() == 'string'
		assert builtin_type_payloads_are_identical(integer_type, tc.resolve_type_uncached(integer))
		assert builtin_type_payloads_are_identical(boolean_type, tc.resolve_type_uncached(boolean))
		assert builtin_type_payloads_are_identical(string_type, tc.resolve_type_uncached(text))
	}
}

fn test_cached_builtin_int_keeps_platform_width_dependent_lowering() {
	saved_bits := platform_int_bits()
	defer { set_platform_int_bits(saved_bits) }
	mut a := flat.FlatAst.new()
	integer := a.add_val(.int_literal, '1')
	for bits in [32, 64, 32, 64] {
		set_platform_int_bits(bits)
		tc := TypeChecker.new(&a)
		typ := tc.resolve_type_uncached(integer)
		assert typ is Primitive
		assert typ.props == .integer
		assert typ.size == 0
		assert typ.name() == 'int'
		assert tc.c_type(typ) == if bits == 32 { 'i32' } else { 'i64' }
		assert builtin_type_payloads_are_identical(typ, builtin_int_type)
	}
}
