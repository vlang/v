module c

import os
import v3.flat
import v3.parser
import v3.pref
import v3.types

fn test_json_helper_scan_requires_legacy_json_module() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	assert !g.has_legacy_json_module()
	tc.file_modules['json_primitives.c.v'] = 'json'
	assert g.has_legacy_json_module()
}

fn test_optional_typedef_collection_ignores_incomplete_call_type_text() {
	mut ast := &flat.FlatAst{}
	ast.nodes = [flat.Node{
		kind: .call
		typ:  '?([]'
	}, flat.Node{
		kind: .call
		typ:  '?string'
	}]
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.collect_optional_typedefs()
	assert 'Optional_string' in g.needed_optional_types
	assert g.needed_optional_types.len == 1
}

fn test_json_pointer_sum_variants_use_direct_owned_payloads() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.sum_types['main.Payload'] = ['&main.Node', 'string']
	tc.structs['main.Node'] = [
		types.StructField{
			name: 'name'
			typ:  types.Type(types.String{})
		},
	]
	mut encode_gen := FlatGen.new()
	encode_gen.a = &ast
	encode_gen.tc = &tc
	payload_type := types.Type(types.SumType{
		name: 'main.Payload'
	})
	pointer_field := encode_gen.sum_field_name('&main.Node')
	encoded := encode_gen.json_encode_value_c_expr_inner(payload_type, 'value', []string{}) or {
		assert false, 'pointer sum encoder was not generated'
		return
	}
	assert encoded.contains('(value).${pointer_field}'), encoded
	assert !encoded.contains('(*(value).${pointer_field})'), encoded
	equal := encode_gen.json_encode_equal_c_expr(payload_type, 'left', 'right', []string{}) or {
		assert false, 'pointer sum equality was not generated'
		return
	}
	assert equal.contains('(left).${pointer_field}'), equal
	assert equal.contains('(right).${pointer_field}'), equal
	assert !equal.contains('(*(left).${pointer_field})'), equal
	assert !equal.contains('(*(right).${pointer_field})'), equal

	mut decode_gen := FlatGen.new()
	decode_gen.a = &ast
	decode_gen.tc = &tc
	decode_gen.gen_json_decode_sum_variant_expr('item', 'main.Payload', '&main.Node')
	decoded := decode_gen.sb.str()
	assert decoded.contains('v3_json_decode_ptr_'), decoded
	assert decoded.contains('._pointer_variant_is_owned = true'), decoded
}

fn test_optional_payload_qualifies_concrete_generic_struct() {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	tc.structs['json2.StructKeyDecodeResult_TestEchoArgs'] = []types.StructField{}
	tc.structs['AnyStruct[json2.Any]'] = []types.StructField{}
	tc.struct_modules['AnyStruct[json2.Any]'] = 'main'
	tc.structs['async.Task_mcp__Response'] = []types.StructField{}
	tc.structs['types.Array'] = []types.StructField{}
	tc.file_imports['main.v\njson'] = 'json2'
	tc.cur_file = 'main.v'
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc

	value_type := types.Type(types.Struct{
		name: 'StructKeyDecodeResult_TestEchoArgs'
	})
	pointer_type := types.Type(types.Pointer{
		base_type: types.Type(types.Struct{
			name: 'Task_mcp__Response'
		})
	})
	assert g.optional_payload_c_type(value_type) == 'json2__StructKeyDecodeResult_TestEchoArgs'
	assert g.optional_payload_c_type(pointer_type) == 'async__Task_mcp__Response*'
	alias_payload := g.optional_payload_c_type(types.Type(types.Struct{
		name: 'main.AnyStruct[json.Any]'
	}))
	assert alias_payload == 'AnyStruct_json2__Any'
	assert g.optional_payload_c_type(types.Type(types.Array{
		elem_type: types.Type(types.int_)
	})) == 'Array'
	result_type := types.Type(types.ResultType{
		base_type: value_type
	})
	assert g.concrete_optional_type_name(result_type) == 'Optional_json2__StructKeyDecodeResult_TestEchoArgs'
	assert g.needed_optional_types['Optional_json2__StructKeyDecodeResult_TestEchoArgs'] == 'json2__StructKeyDecodeResult_TestEchoArgs'
}

fn test_value_type_qualifies_concrete_generic_struct() {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	tc.structs['orm.QueryBuilder_User'] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc

	value_type := types.Type(types.Struct{
		name: 'QueryBuilder_User'
	})
	pointer_type := types.Type(types.Pointer{
		base_type: value_type
	})
	assert g.value_c_type(value_type) == 'orm__QueryBuilder_User'
	assert g.value_c_type(pointer_type) == 'orm__QueryBuilder_User*'
}

fn test_optional_payload_ignores_import_aliases_from_other_files() {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	tc.cur_file = 'json/any.v'
	tc.structs['json.Any'] = []types.StructField{}
	tc.structs['json2.Any'] = []types.StructField{}
	tc.structs['AnyStruct[json.Any]'] = []types.StructField{}
	tc.structs['AnyStruct[json2.Any]'] = []types.StructField{}
	tc.file_imports['unrelated.v\njson'] = 'json2'
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc

	alias_payload := g.optional_payload_c_type(types.Type(types.Struct{
		name: 'main.AnyStruct[json.Any]'
	}))
	assert alias_payload == 'main__AnyStruct_json__Any'
}

fn test_optional_payload_qualifies_interface() {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	tc.interface_names['firebird.Value'] = true
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc

	value_type := types.Type(types.Interface{
		name: 'Value'
	})
	assert g.optional_payload_c_type(value_type) == 'firebird__Value'
	result_type := types.Type(types.ResultType{
		base_type: value_type
	})
	assert g.concrete_optional_type_name(result_type) == 'Optional_firebird__Value'
}

fn test_optional_payload_keeps_concrete_c_type_with_interface_collision() {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	tc.interface_names['pkg.Value'] = true
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc

	value_type := types.Type(types.Struct{
		name: 'C.Value'
	})
	assert g.value_c_type(value_type) == 'Value'
	assert g.optional_payload_c_type(value_type) == 'Value'
	result_type := types.Type(types.ResultType{
		base_type: value_type
	})
	assert g.concrete_optional_type_name(result_type) == 'Optional_Value'
}

fn test_optional_payload_does_not_qualify_ambiguous_interface() {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	tc.cur_module = 'json2'
	tc.interface_names['firebird.Value'] = true
	tc.interface_names['json2.Value'] = true
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc

	value_type := types.Type(types.Interface{
		name: 'Value'
	})
	assert g.optional_payload_c_type(value_type) == 'Value'
	assert g.stale_ambiguous_qualified_interface_c_type('Value')
}

fn test_declaration_signature_scan_ignores_unscoped_regular_fn_nodes() {
	mut ast := flat.FlatAst.new()
	ast.add_node(flat.Node{
		kind:  .fn_decl
		value: 'load'
		typ:   '!Image'
	})
	mut tc := types.TypeChecker.new(&ast)
	tc.cur_module = 'json2'
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc

	g.collect_declaration_signature_types()
	assert 'Optional_json2__Image' !in g.needed_optional_types
}

fn test_declaration_signature_scan_collects_specialized_fn_nodes() {
	mut ast := flat.FlatAst.new()
	fn_id := ast.add_node(flat.Node{
		kind:  .fn_decl
		value: 'decode_T_Data'
		typ:   '!Data'
	})
	ast.specialized_fn_nodes[int(fn_id)] = true
	mut tc := types.TypeChecker.new(&ast)
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc

	g.collect_declaration_signature_types()
	assert 'Optional_Data' in g.needed_optional_types
}

fn test_specialized_signature_scan_uses_declaration_module() {
	mut ast := flat.FlatAst.new()
	fn_id := ast.add_node(flat.Node{
		kind:  .fn_decl
		value: 'QueryBuilder_Entity_update'
		typ:   '!&QueryBuilder[Entity]'
	})
	ast.specialized_fn_nodes[int(fn_id)] = true
	ast.specialized_fn_modules[int(fn_id)] = 'orm'
	mut tc := types.TypeChecker.new(&ast)
	tc.structs['Entity'] = []types.StructField{}
	tc.structs['orm.QueryBuilder[Entity]'] = []types.StructField{}
	tc.struct_modules['orm.QueryBuilder'] = 'orm'
	tc.struct_modules['orm.QueryBuilder[Entity]'] = 'orm'
	tc.struct_generic_params['orm.QueryBuilder'] = ['T']
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc

	g.collect_declaration_signature_types()
	assert 'Optional_orm__QueryBuilder_Entityptr' in g.needed_optional_types
	assert 'Optional_QueryBuilder_Entityptr' !in g.needed_optional_types
}

fn test_optional_value_info_preserves_pointer_payload_abi() {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc

	option_type := types.Type(types.OptionType{
		base_type: types.Type(types.Struct{
			name: 'Data'
		})
	})
	payload_ct, payload_type := g.optional_value_info(option_type, 'Optional_Dataptr')
	assert payload_ct == 'Data*'
	assert payload_type is types.Pointer
	assert (payload_type as types.Pointer).base_type.name() == 'Data'
}

fn test_array_equality_depth_follows_the_resolved_element_type() {
	mut elem_type := types.Type(types.int_)
	for _ in 0 .. 8 {
		elem_type = types.Type(types.Array{
			elem_type: elem_type
		})
	}
	assert array_equality_depth_from_elem_type(elem_type) == 9
}

fn test_enum_decls_resets_checker_module_at_file_boundary() {
	test_dir := os.join_path(os.vtmp_dir(), 'v3_enum_decls_module_reset_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	main_path := os.join_path(test_dir, 'main.v')
	shadow_path := os.join_path(test_dir, 'shadow.v')
	os.write_file(main_path, 'module main

type Storage = u64

const base = 300

enum Example as Storage {
	a = base + 2
	b
}
') or {
		panic(err)
	}
	os.write_file(shadow_path, 'module shadow

type Storage = u8

const base = 4
') or {
		panic(err)
	}

	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_files([main_path, shadow_path])
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	tc.cur_file = main_path
	tc.cur_module = 'shadow'

	mut g := FlatGen.new()
	g.a = a
	g.tc = &tc
	g.enum_decls()
	c_source := g.sb.str()
	assert c_source.contains('typedef u64 Example;'), c_source
	assert c_source.contains('#define Example__a ((Example)(302))'), c_source
	assert tc.cur_module == 'shadow'
}
