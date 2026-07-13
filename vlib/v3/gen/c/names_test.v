module c

import v3.flat
import v3.types

// test_c_name_sanitize_operator_overloads validates this v3 regression case.
fn test_c_name_sanitize_operator_overloads() {
	assert c_name('Point.<') == 'Point__lt'
	assert c_name('Point.<=') == 'Point__le'
	assert c_name('Point.>') == 'Point__gt'
	assert c_name('Point.>=') == 'Point__ge'
}

fn test_c_name_sanitize_escaped_keywords() {
	assert c_name('@true') == '_v_true'
	assert c_name('@false') == '_v_false'
	assert c_name('Kind.@asm') == 'Kind___v_asm'
}

fn test_c_name_libc_collision_abs() {
	assert c_name('abs') == 'v_abs'
	assert c_name('C.abs') == 'abs'
}

fn test_cgen_flattened_generic_receiver_short_variants() {
	assert cgen_flattened_generic_receiver_short_variants('foo__Bar_baz__Qux') == [
		'Bar_Qux',
	]
	assert cgen_flattened_generic_receiver_short_variants('mod.foo__Bar_baz__Qux') == [
		'Bar_Qux',
		'mod.Bar_Qux',
	]
}

fn test_cgen_typeof_display_canonicalizes_fixed_array_generic_args() {
	assert typeof_display_type_name('Box[fn () int]') == 'Box[fn () int]'
	assert typeof_display_type_name('Box[chan int]') == 'Box[chan int]'
	assert typeof_display_type_name('chan int[3]') == 'chan [3]int'
	assert typeof_display_type_name('Box[int[3]]') == 'Box[[3]int]'
	assert typeof_display_type_name('Pair[int[3], Box[string[2]]]') == 'Pair[[3]int, Box[[2]string]]'
	assert typeof_display_type_name('Box[int][3]') == '[3]Box[int]'
	fixed_maps := types.Type(types.ArrayFixed{
		elem_type: types.Type(types.Map{
			key_type:   types.Type(types.String{})
			value_type: types.Type(types.int_)
		})
		len:       3
	})
	assert typeof_display_resolved_type_name(fixed_maps) == '[3]map[string]int'
}

fn test_sum_type_index_rejects_ambiguous_qualified_suffix() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types['a.tast.Value'] = ['a.tast.First', 'a.tast.Target']
	tc.sum_types['b.tast.Value'] = ['b.tast.Target', 'b.tast.Second']
	mut g := FlatGen.new()
	g.tc = &tc
	assert g.sum_type_index('tast.Value', 'b.tast.Target') == 0
}

fn test_fn_decl_variadic_resolves_alias_before_short_fallback() {
	mut g := FlatGen.new()
	g.modules['http'] = 'b.http'
	g.fn_decl_variadic['b.http.total'] = true
	g.fn_decl_variadic['total'] = false
	g.fn_decl_variadic_short_counts['total'] = 2
	assert g.fn_decl_is_variadic('http.total', 'http.total')
	assert !g.fn_decl_is_variadic('missing.total', 'missing.total')
	assert !g.fn_decl_is_variadic('missing__total', 'total')
	g.fn_decl_variadic_short_counts['total'] = 1
	g.fn_decl_variadic['total'] = true
	assert g.fn_decl_is_variadic('missing.total', 'missing.total')
}

fn test_guarded_preamble_externs_keep_explicit_declarations() {
	g := FlatGen.new()
	assert g.should_emit_c_extern_decl('fseeko')
	assert g.should_emit_c_extern_decl('ftello')
	assert g.should_emit_c_extern_decl('mkdir')
	assert g.should_emit_c_extern_decl('chmod')
	assert g.should_emit_c_extern_decl('symlink')
}
