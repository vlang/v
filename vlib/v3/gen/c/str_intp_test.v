module c

import v3.flat
import v3.types

fn formatted_enum_interp_c_expr(format string) string {
	mut a := flat.FlatAst.new()
	value_id := a.add_val(.int_literal, '5')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.gen_formatted_string_interp_child_expr(value_id, types.Enum{
		name: 'Color'
	}, format)
	return g.sb.str()
}

fn formatted_u8_interp_c_expr(format string) string {
	mut a := flat.FlatAst.new()
	value_id := a.add_val(.int_literal, '102')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.gen_formatted_string_interp_child_expr(value_id, types.Type(types.u8_), format)
	return g.sb.str()
}

fn test_string_literal_table_has_internal_const_linkage() {
	mut g := FlatGen.new()
	g.intern_string('literal')
	g.string_literals()
	assert g.sb.str() == 'static const string _str_0 = {"literal", 7, 1};\n\n'
}

fn test_character_interpolation_uses_rune_text() {
	assert formatted_u8_interp_c_expr('1c') == 'rune__str((u32)(102))'
	assert formatted_u8_interp_c_expr('3c') == 'v3_string_pad(rune__str((u32)(102)), 3, 0)'
}

fn test_character_interpolation_unwraps_integer_alias() {
	mut a := flat.FlatAst.new()
	value_id := a.add_val(.int_literal, '65')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.gen_formatted_string_interp_child_expr(value_id, types.Alias{
		name:      'Code'
		base_type: types.Type(types.u8_)
	}, 'c')
	assert g.sb.str() == 'rune__str((u32)(65))'
}

fn test_width_only_enum_interpolation_uses_enum_text() {
	assert formatted_enum_interp_c_expr('8') == 'v3_string_pad(Color__autostr(5), 8, 0)'
	assert formatted_enum_interp_c_expr('-8') == 'v3_string_pad(Color__autostr(5), 8, 1)'
	assert formatted_enum_interp_c_expr('08') == 'v3_string_zpad(Color__autostr(5), 8)'
	assert formatted_enum_interp_c_expr('8d') == 'v3_string_pad(i64__str((i64)(5)), 8, 0)'
}

fn test_ierror_interpolation_uses_dynamic_message_dispatch() {
	mut a := flat.FlatAst.new()
	err_id := a.add_node(flat.Node{
		kind:  .ident
		value: 'err'
		typ:   'IError'
	})
	a.children << err_id
	interp_id := a.add_node(flat.Node{
		kind:           .string_interp
		children_start: 0
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.register_synth_type(err_id, types.Type(types.Interface{
		name: 'IError'
	}))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.gen_string_interp(a.nodes[int(interp_id)])
	assert g.sb.str() == 'string_plus_many(1, (string[1]){({ IError _ierror_msg0 = err; IError__msg(&_ierror_msg0); })})'
}
