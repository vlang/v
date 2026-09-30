module c

import v.flat
import v.types

fn test_option_and_result_have_distinct_layouts() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.interface_names['IError'] = true
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.interfaces['IError'] = []string{}
	option := types.Type(types.OptionType{ base_type: types.Type(types.i64_) })
	result := types.Type(types.ResultType{ base_type: types.Type(types.i64_) })
	option_name := g.optional_type_name(option)
	result_name := g.optional_type_name(result)
	assert option_name != result_name
	assert g.emit_optional_typedef(option_name, 'i64')
	assert g.emit_optional_typedef(result_name, 'i64')
	assert g.sb.str().split_into_lines() == [
		'typedef struct __v_option_i64 { bool ok; i64 value; } __v_option_i64;',
		'typedef struct __v_result_i64 { bool ok; IError err; i64 value; } __v_result_i64;',
	]
}
