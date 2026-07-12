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

fn test_width_only_enum_interpolation_uses_enum_text() {
	assert formatted_enum_interp_c_expr('8') == 'v3_string_pad(Color__autostr(5), 8, 0)'
	assert formatted_enum_interp_c_expr('-8') == 'v3_string_pad(Color__autostr(5), 8, 1)'
	assert formatted_enum_interp_c_expr('08') == 'v3_string_zpad(Color__autostr(5), 8)'
	assert formatted_enum_interp_c_expr('8d') == 'v3_string_pad(i64__str((i64)(5)), 8, 0)'
}
