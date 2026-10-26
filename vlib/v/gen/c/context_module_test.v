module c

import v.flat
import v.types

fn test_generated_capture_struct_uses_source_module_for_fixed_array_size() {
	mut a := flat.FlatAst.new()
	value := a.add_val(.int_literal, '4')
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'cached_support'
	tc.const_exprs['fixed_array_size'] = value
	tc.const_modules['fixed_array_size'] = 'main'
	context_name := '__anon_fn_0_Ctx'
	tc.struct_modules[context_name] = 'main'
	tc.structs[context_name] = [types.StructField{
		name: 'values'
		typ:  types.Type(types.Pointer{
			base_type: types.Type(types.ArrayFixed{
				elem_type: types.Type(types.int_)
				len_expr:  'fixed_array_size'
			})
		})
	}]
	// Simulate the spelling first requested while generating cached support code.
	assert tc.c_type(tc.structs[context_name][0].typ) == 'Array_fixed_i64_fixed_array_size*'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.emit_struct(context_name)
	assert g.sb.str().contains('Array_fixed_i64_4* values;'), g.sb.str()
	assert tc.cur_module == 'cached_support'
}

fn test_callback_field_lookup_preserves_generic_argument_module() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'genericmodule'
	caller_callback := types.Type(types.FnType{ return_type: types.Type(types.int_) })
	imported_callback := types.Type(types.FnType{ return_type: types.Type(types.string_) })
	tc.structs['genericmodule.Each[Row]'] = [types.StructField{
		name: 'visit'
		typ:  caller_callback
	}]
	tc.structs['genericmodule.Each[genericmodule.Row]'] = [types.StructField{
		name: 'visit'
		typ:  imported_callback
	}]
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	caller := types.Type(types.Struct{ name_id: flat.source_type_id('genericmodule.Each[main.Row]') })
	imported := types.Type(types.Struct{ name_id: flat.source_type_id('genericmodule.Each[genericmodule.Row]') })
	assert g.field_type(caller, 'visit')? == caller_callback
	assert g.field_type(imported, 'visit')? == imported_callback
	assert g.field_type(caller, 'missing') == none
	// An exact specialization takes precedence over the bare main spelling.
	tc.structs['genericmodule.Each[main.Row]'] = [types.StructField{
		name: 'visit'
		typ:  imported_callback
	}]
	assert g.field_type(caller, 'visit')? == imported_callback
}
