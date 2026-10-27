module transform

import v.flat
import v.types

fn test_specialized_receiver_callback_uses_function_type_compatibility() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'callback')
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('SharedCb'), flat.source_type_id('fn (shared int)'))
	tc.type_aliases.set_id(flat.source_type_id('PlainCb'), flat.source_type_id('fn (int)'))
	tc.type_aliases.set_id(flat.source_type_id('SharedHandlers'), flat.source_type_id('map[string]fn (shared int)'))
	tc.type_aliases.set_id(flat.source_type_id('PlainHandlers'), flat.source_type_id('map[string]fn (int)'))
	tc.type_aliases.set_id(flat.source_type_id('FastFn'), flat.source_type_id('fn (int)'))
	tc.type_aliases.set_id(flat.source_type_id('CdeclFn'), flat.source_type_id('fn (int)'))
	tc.type_aliases.set_id(flat.source_type_id('FastBox'), flat.source_type_id('Box[FastFn]'))
	tc.type_aliases.set_id(flat.source_type_id('CdeclBox'), flat.source_type_id('Box[CdeclFn]'))
	fast_decl := a.add_node(flat.Node{ kind: .type_decl, value: 'FastFn' })
	cdecl_decl := a.add_node(flat.Node{ kind: .type_decl, value: 'CdeclFn' })
	tc.type_declaration_ids[flat.source_type_id('FastFn')] = [int(fast_decl)]
	tc.type_declaration_ids[flat.source_type_id('CdeclFn')] = [int(cdecl_decl)]
	tc.declaration_attributes[int(fast_decl)] = ['callconv: fastcall']
	tc.declaration_attributes[int(cdecl_decl)] = ['callconv: cdecl']
	tc.structs[flat.source_type_id('Box')] = []types.StructField{}
	tc.struct_generic_params[flat.source_type_id('Box')] = ['T']
	tc.structs[flat.source_type_id('Pair')] = []types.StructField{}
	tc.struct_generic_params[flat.source_type_id('Pair')] = ['T', 'U']
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([]int, []int) int'), flat.source_type_id('fn([]int, []int) int'))
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (values []f64, indices []int) f64'), flat.source_type_id('fn([]f64, []int) f64'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([]int) int'), flat.source_type_id('fn([]int, []int) int'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([]string)'), flat.source_type_id('fn ([]int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([]i32)'), flat.source_type_id('fn ([]i64)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([][]i32)'), flat.source_type_id('fn ([][]i64)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (map[string]string)'),
		flat.source_type_id('fn (map[string]int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (map[string]i32)'),
		flat.source_type_id('fn (map[string]i64)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (chan string)'), flat.source_type_id('fn (chan int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (chan i32)'), flat.source_type_id('fn (chan i64)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (?string)'), flat.source_type_id('fn (?int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (!string)'), flat.source_type_id('fn (!int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (...int)'), flat.source_type_id('fn ([]int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([]int)'), flat.source_type_id('fn (...int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (fn (...int))'), flat.source_type_id('fn (fn ([]int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([]int, []int) string'), flat.source_type_id('fn([]int, []int) int'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (mut []int) int'), flat.source_type_id('fn([]int) int'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (shared int)'), flat.source_type_id('fn (int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (int)'), flat.source_type_id('fn (shared int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (atomic int)'), flat.source_type_id('fn (int)'))
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (shared int)'), flat.source_type_id('fn (shared int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (fn (shared int))'),
		flat.source_type_id('fn (fn (int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (fn (int))'), flat.source_type_id('fn (fn (atomic int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn () fn (shared int)'),
		flat.source_type_id('fn () fn (int)'))
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (fn (shared int))'),
		flat.source_type_id('fn (fn (shared int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([]fn (shared int))'),
		flat.source_type_id('fn ([]fn (int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (...fn (shared int))'),
		flat.source_type_id('fn (...fn (int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([2]fn (atomic int))'),
		flat.source_type_id('fn ([2]fn (int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (map[string]fn (shared int))'),
		flat.source_type_id('fn (map[string]fn (int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (SharedCb)'), flat.source_type_id('fn (PlainCb)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (FastFn)'), flat.source_type_id('fn (CdeclFn)'))
	for wrapper in ['Box[%s]', 'Pair[int, %s]', 'Box[[]%s]', '[]Box[%s]', '?Box[%s]',
		'map[string]Box[%s]'] {
		actual := wrapper.replace('%s', 'FastFn')
		expected := wrapper.replace('%s', 'CdeclFn')
		assert !tc.fn_type_callconv_compatible(tc.parse_type(flat.source_type_id('fn (${actual})')), tc.parse_type(flat.source_type_id('fn (${expected})')))
		assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (${actual})'), flat.source_type_id('fn (${expected})'))
		assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn () ${actual}'), flat.source_type_id('fn () ${expected}'))
		assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (${actual})'), flat.source_type_id('fn (${actual})'))
	}
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (FastBox)'), flat.source_type_id('fn (CdeclBox)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([]FastFn)'), flat.source_type_id('fn ([]CdeclFn)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn ([2]FastFn)'), flat.source_type_id('fn ([2]CdeclFn)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (map[string]FastFn)'),
		flat.source_type_id('fn (map[string]CdeclFn)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (chan FastFn)'), flat.source_type_id('fn (chan CdeclFn)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (thread FastFn)'), flat.source_type_id('fn (thread CdeclFn)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (&FastFn)'), flat.source_type_id('fn (&CdeclFn)'))
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (FastFn)'), flat.source_type_id('fn (FastFn)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn () SharedCb'), flat.source_type_id('fn () PlainCb'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (SharedHandlers)'),
		flat.source_type_id('fn (PlainHandlers)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (chan fn (shared int))'),
		flat.source_type_id('fn (chan fn (int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (thread fn (shared int))'),
		flat.source_type_id('fn (thread fn (int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (Box[fn (shared int)])'),
		flat.source_type_id('fn (Box[fn (int)])'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (Pair[fn (int), fn (shared int)])'),
		flat.source_type_id('fn (Pair[fn (int), fn (int)])'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (shared handlers []fn (shared int))'),
		flat.source_type_id('fn (shared handlers []fn (int))'))
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (shared handlers []fn (shared int))'),
		flat.source_type_id('fn (shared handlers []fn (shared int))'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn () (fn (shared int), int)'),
		flat.source_type_id('fn () (fn (int), int)'))
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn () (fn (shared int), int)'),
		flat.source_type_id('fn () (fn (shared int), int)'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn () shared int'), flat.source_type_id('fn () int'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn () int'), flat.source_type_id('fn () atomic int'))
	t.set_node_typ(int(id), flat.source_type_id('FastFn'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (int)'), flat.source_type_id('CdeclFn'))
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (int)'), flat.source_type_id('FastFn'))
}

fn test_specialized_callbacks_preserve_abi_identical_scalar_payloads() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'callback')
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	for wrapper in ['&%s', '[]%s', 'map[string]%s', 'chan %s', '?%s', '!%s', '[][2]%s'] {
		actual := 'fn (${wrapper.replace('%s', 'rune')})'
		expected := 'fn (${wrapper.replace('%s', 'u32')})'
		assert tc.slot_value_compatible(tc.parse_type(flat.source_type_id(actual)), tc.parse_type(flat.source_type_id(expected)))
		assert t.resolved_receiver_arg_compatible(id, flat.source_type_id(actual), flat.source_type_id(expected))
		assert t.resolved_receiver_arg_compatible(id, flat.source_type_id(expected), flat.source_type_id(actual))
		assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id(actual), flat.source_type_id('fn (${wrapper.replace('%s', 'u64')})'))
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
		assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id(mutable), flat.source_type_id(immutable))
		assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id(immutable), flat.source_type_id(mutable))
		assert t.resolved_receiver_arg_compatible(id, flat.source_type_id(mutable), flat.source_type_id(mutable))
		referenced := 'fn (${wrapper.replace('%s', 'fn (&int)')})'
		assert t.resolved_receiver_arg_compatible(id, flat.source_type_id(mutable), flat.source_type_id(referenced)) == tc.slot_value_compatible(tc.parse_type(flat.source_type_id(mutable)), tc.parse_type(flat.source_type_id(referenced)))
		assert t.resolved_receiver_arg_compatible(id, flat.source_type_id(referenced), flat.source_type_id(mutable)) == tc.slot_value_compatible(tc.parse_type(flat.source_type_id(referenced)), tc.parse_type(flat.source_type_id(mutable)))
		typed_return := 'fn (${wrapper.replace('%s', 'fn () &int')})'
		void_return := 'fn (${wrapper.replace('%s', 'fn () voidptr')})'
		assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id(typed_return), flat.source_type_id(void_return))
		assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id(void_return), flat.source_type_id(typed_return))
	}
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (&int)'), flat.source_type_id('fn (voidptr)'))
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn (voidptr)'), flat.source_type_id('fn (&int)'))
	assert t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn () &int'), flat.source_type_id('fn () voidptr'))
	assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id('fn () voidptr'), flat.source_type_id('fn () &int'))
}

fn test_nested_callback_payloads_use_checker_parameter_modes() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	t := new_transformer(mut a, &tc, map[string]bool{})
	integer := tc.parse_type(flat.source_type_id('int'))
	pointer := tc.parse_type(flat.source_type_id('&int'))
	result := tc.intern_type_reference(types.Type(types.void_))
	mutable := tc.fn_type([integer], result, [true])
	immutable := tc.fn_type([integer], result, []bool{})
	referenced := tc.fn_type([pointer], result, []bool{})
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
		tc.type_aliases.set_id(flat.source_type_id(alias), flat.source_type_id(forms[0]))
		tc.type_aliases.set_id(flat.source_type_id(chain), flat.source_type_id(alias))
		for raw in [alias, chain] {
			id := t.a.add_node(flat.Node{ kind: .ident, value: 'callback${idx}${raw}', typ: flat.source_type_id(raw) })
			assert !t.resolved_receiver_arg_compatible(id, flat.source_type_id(forms[1]), flat.source_type_id(forms[1])), raw
			assert t.resolved_receiver_arg_compatible(id, flat.source_type_id(forms[1]), flat.source_type_id(alias)), raw
			assert t.resolved_receiver_arg_compatible(id, flat.source_type_id(forms[1]), flat.source_type_id(raw)), raw
		}
	}
	tc.type_aliases.set_id(flat.source_type_id('UnrelatedCallback'), flat.source_type_id('fn (shared string)'))
	unrelated := t.a.add_node(flat.Node{ kind: .ident, value: 'unrelated', typ: flat.source_type_id('UnrelatedCallback') })
	assert t.resolved_receiver_arg_compatible(unrelated, flat.source_type_id('fn (int)'), flat.source_type_id('fn (int)'))
}
