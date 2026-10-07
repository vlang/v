module transform

import v.flat
import v.types

fn test_specialized_receiver_callback_uses_function_type_compatibility() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'callback')
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['SharedCb'] = 'fn (shared int)'
	tc.type_aliases['PlainCb'] = 'fn (int)'
	tc.type_aliases['SharedHandlers'] = 'map[string]fn (shared int)'
	tc.type_aliases['PlainHandlers'] = 'map[string]fn (int)'
	tc.type_aliases['FastFn'] = 'fn (int)'
	tc.type_aliases['CdeclFn'] = 'fn (int)'
	tc.type_aliases['FastBox'] = 'Box[FastFn]'
	tc.type_aliases['CdeclBox'] = 'Box[CdeclFn]'
	fast_decl := a.add_node(flat.Node{ kind: .type_decl, value: 'FastFn' })
	cdecl_decl := a.add_node(flat.Node{ kind: .type_decl, value: 'CdeclFn' })
	tc.type_declaration_ids['FastFn'] = [int(fast_decl)]
	tc.type_declaration_ids['CdeclFn'] = [int(cdecl_decl)]
	tc.declaration_attributes[int(fast_decl)] = ['callconv: fastcall']
	tc.declaration_attributes[int(cdecl_decl)] = ['callconv: cdecl']
	tc.structs['Box'] = []types.StructField{}
	tc.struct_generic_params['Box'] = ['T']
	tc.structs['Pair'] = []types.StructField{}
	tc.struct_generic_params['Pair'] = ['T', 'U']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.resolved_receiver_arg_compatible(id, 'fn ([]int, []int) int', 'fn([]int, []int) int')
	assert t.resolved_receiver_arg_compatible(id, 'fn (values []f64, indices []int) f64', 'fn([]f64, []int) f64')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]int) int', 'fn([]int, []int) int')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]string)', 'fn ([]int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]i32)', 'fn ([]i64)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([][]i32)', 'fn ([][]i64)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (map[string]string)',
		'fn (map[string]int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (map[string]i32)',
		'fn (map[string]i64)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (chan string)', 'fn (chan int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (chan i32)', 'fn (chan i64)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (?string)', 'fn (?int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (!string)', 'fn (!int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (...int)', 'fn ([]int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]int)', 'fn (...int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (fn (...int))', 'fn (fn ([]int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]int, []int) string', 'fn([]int, []int) int')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (mut []int) int', 'fn([]int) int')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (shared int)', 'fn (int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (int)', 'fn (shared int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (atomic int)', 'fn (int)')
	assert t.resolved_receiver_arg_compatible(id, 'fn (shared int)', 'fn (shared int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (fn (shared int))',
		'fn (fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (fn (int))', 'fn (fn (atomic int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn () fn (shared int)',
		'fn () fn (int)')
	assert t.resolved_receiver_arg_compatible(id, 'fn (fn (shared int))',
		'fn (fn (shared int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]fn (shared int))',
		'fn ([]fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (...fn (shared int))',
		'fn (...fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([2]fn (atomic int))',
		'fn ([2]fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (map[string]fn (shared int))',
		'fn (map[string]fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (SharedCb)', 'fn (PlainCb)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (FastFn)', 'fn (CdeclFn)')
	for wrapper in ['Box[%s]', 'Pair[int, %s]', 'Box[[]%s]', '[]Box[%s]', '?Box[%s]',
		'map[string]Box[%s]'] {
		actual := wrapper.replace('%s', 'FastFn')
		expected := wrapper.replace('%s', 'CdeclFn')
		assert !tc.fn_type_callconv_compatible(tc.parse_type('fn (${actual})'), tc.parse_type('fn (${expected})'))
		assert !t.resolved_receiver_arg_compatible(id, 'fn (${actual})', 'fn (${expected})')
		assert !t.resolved_receiver_arg_compatible(id, 'fn () ${actual}', 'fn () ${expected}')
		assert t.resolved_receiver_arg_compatible(id, 'fn (${actual})', 'fn (${actual})')
	}
	assert !t.resolved_receiver_arg_compatible(id, 'fn (FastBox)', 'fn (CdeclBox)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([]FastFn)', 'fn ([]CdeclFn)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn ([2]FastFn)', 'fn ([2]CdeclFn)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (map[string]FastFn)',
		'fn (map[string]CdeclFn)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (chan FastFn)', 'fn (chan CdeclFn)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (thread FastFn)', 'fn (thread CdeclFn)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (&FastFn)', 'fn (&CdeclFn)')
	assert t.resolved_receiver_arg_compatible(id, 'fn (FastFn)', 'fn (FastFn)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn () SharedCb', 'fn () PlainCb')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (SharedHandlers)',
		'fn (PlainHandlers)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (chan fn (shared int))',
		'fn (chan fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (thread fn (shared int))',
		'fn (thread fn (int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (Box[fn (shared int)])',
		'fn (Box[fn (int)])')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (Pair[fn (int), fn (shared int)])',
		'fn (Pair[fn (int), fn (int)])')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (shared handlers []fn (shared int))',
		'fn (shared handlers []fn (int))')
	assert t.resolved_receiver_arg_compatible(id, 'fn (shared handlers []fn (shared int))',
		'fn (shared handlers []fn (shared int))')
	assert !t.resolved_receiver_arg_compatible(id, 'fn () (fn (shared int), int)',
		'fn () (fn (int), int)')
	assert t.resolved_receiver_arg_compatible(id, 'fn () (fn (shared int), int)',
		'fn () (fn (shared int), int)')
	assert !t.resolved_receiver_arg_compatible(id, 'fn () shared int', 'fn () int')
	assert !t.resolved_receiver_arg_compatible(id, 'fn () int', 'fn () atomic int')
	t.set_node_typ(int(id), 'FastFn')
	assert !t.resolved_receiver_arg_compatible(id, 'fn (int)', 'CdeclFn')
	assert t.resolved_receiver_arg_compatible(id, 'fn (int)', 'FastFn')
}

fn test_specialized_callbacks_preserve_abi_identical_scalar_payloads() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'callback')
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for wrapper in ['&%s', '[]%s', 'map[string]%s', 'chan %s', '?%s', '!%s', '[][2]%s'] {
		actual := 'fn (${wrapper.replace('%s', 'rune')})'
		expected := 'fn (${wrapper.replace('%s', 'u32')})'
		assert tc.slot_value_compatible(tc.parse_type(actual), tc.parse_type(expected))
		assert t.resolved_receiver_arg_compatible(id, actual, expected)
		assert t.resolved_receiver_arg_compatible(id, expected, actual)
		assert !t.resolved_receiver_arg_compatible(id, actual, 'fn (${wrapper.replace('%s', 'u64')})')
	}
}

fn test_nested_callback_parameter_modes_and_return_pointer_identity() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'callback')
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for wrapper in ['%s', '[]%s', 'map[string]%s', '?%s', '!%s'] {
		mutable := 'fn (${wrapper.replace('%s', 'fn (mut int)')})'
		immutable := 'fn (${wrapper.replace('%s', 'fn (int)')})'
		assert !t.resolved_receiver_arg_compatible(id, mutable, immutable)
		assert !t.resolved_receiver_arg_compatible(id, immutable, mutable)
		assert t.resolved_receiver_arg_compatible(id, mutable, mutable)
		referenced := 'fn (${wrapper.replace('%s', 'fn (&int)')})'
		assert t.resolved_receiver_arg_compatible(id, mutable, referenced) == tc.slot_value_compatible(tc.parse_type(mutable), tc.parse_type(referenced))
		assert t.resolved_receiver_arg_compatible(id, referenced, mutable) == tc.slot_value_compatible(tc.parse_type(referenced), tc.parse_type(mutable))
		typed_return := 'fn (${wrapper.replace('%s', 'fn () &int')})'
		void_return := 'fn (${wrapper.replace('%s', 'fn () voidptr')})'
		assert !t.resolved_receiver_arg_compatible(id, typed_return, void_return)
		assert !t.resolved_receiver_arg_compatible(id, void_return, typed_return)
	}
	assert t.resolved_receiver_arg_compatible(id, 'fn (&int)', 'fn (voidptr)')
	assert t.resolved_receiver_arg_compatible(id, 'fn (voidptr)', 'fn (&int)')
	assert t.resolved_receiver_arg_compatible(id, 'fn () &int', 'fn () voidptr')
	assert !t.resolved_receiver_arg_compatible(id, 'fn () voidptr', 'fn () &int')
}

fn test_nested_callback_payloads_use_checker_parameter_modes() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})
	mutable := types.FnType{ params: [types.Type(types.int_)], params_mut: [true], return_type: types.Type(types.void_) }
	immutable := types.FnType{ params: [types.Type(types.int_)], return_type: types.Type(types.void_) }
	referenced := types.FnType{ params: [types.Type(types.Pointer{ base_type: types.Type(types.int_) })], return_type: types.Type(types.void_) }
	assert !t.callback_fn_type_payloads_compatible(mutable, immutable, true)
	assert !t.callback_fn_type_payloads_compatible(immutable, mutable, true)
	assert t.callback_fn_type_payloads_compatible(mutable, referenced, true)
	assert t.callback_fn_type_payloads_compatible(referenced, mutable, true)
}

fn test_specialized_callbacks_keep_alias_modes_after_semantic_resolution() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for idx, forms in [
		['fn (shared int)', 'fn (int)'],
		['fn (atomic int)', 'fn (int)'],
		['fn (...int)', 'fn ([]int)'],
		['fn (fn (shared int))', 'fn (fn (int))'],
		['fn (fn (...int))', 'fn (fn ([]int))'],
		['fn ([]fn (...int))', 'fn ([]fn ([]int))'],
		['fn () fn (...int)', 'fn () fn ([]int)'],
	] {
		alias := 'ModeCallback${idx}'
		chain := 'ChainedCallback${idx}'
		tc.type_aliases[alias] = forms[0]
		tc.type_aliases[chain] = alias
		for raw in [alias, chain] {
			id := t.a.add_node(flat.Node{ kind: .ident, value: 'callback${idx}${raw}', typ: raw })
			assert !t.resolved_receiver_arg_compatible(id, forms[1], forms[1]), raw
			assert t.resolved_receiver_arg_compatible(id, forms[1], alias), raw
			assert t.resolved_receiver_arg_compatible(id, forms[1], raw), raw
		}
	}
	tc.type_aliases['UnrelatedCallback'] = 'fn (shared string)'
	unrelated := t.a.add_node(flat.Node{ kind: .ident, value: 'unrelated', typ: 'UnrelatedCallback' })
	assert t.resolved_receiver_arg_compatible(unrelated, 'fn (int)', 'fn (int)')
}
