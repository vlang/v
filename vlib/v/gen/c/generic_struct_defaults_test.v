module c

import v.flat
import v.types

fn test_flattened_generic_struct_default_value_preserves_field_defaults() {
	mut ast := flat.FlatAst.new()
	default_value := ast.add_node(flat.Node{
		kind: .int_literal
		value: '5'
	})
	field_start := ast.children.len
	ast.children << default_value
	x_field := ast.add_node(flat.Node{
		kind: .field_decl
		value: 'x'
		typ: 'int'
		children_start: field_start
		children_count: 1
	})
	items_field := ast.add_node(flat.Node{
		kind: .field_decl
		value: 'items'
		typ: '[]T'
	})
	channel_cap := ast.add_node(flat.Node{
		kind: .int_literal
		value: '1'
	})
	channel_cap_start := ast.children.len
	ast.children << channel_cap
	channel_cap_field := ast.add_node(flat.Node{
		kind: .field_init
		value: 'cap'
		children_start: channel_cap_start
		children_count: 1
	})
	channel_init_start := ast.children.len
	ast.children << channel_cap_field
	channel_init := ast.add_node(flat.Node{
		kind: .struct_init
		value: 'chan T'
		typ: 'chan T'
		children_start: channel_init_start
		children_count: 1
	})
	channel_field_start := ast.children.len
	ast.children << channel_init
	channel_field := ast.add_node(flat.Node{
		kind: .field_decl
		value: 'ch'
		typ: 'chan T'
		children_start: channel_field_start
		children_count: 1
	})
	size_value := ast.add_node(flat.Node{
		kind: .sizeof_expr
		value: 'T'
	})
	size_field_start := ast.children.len
	ast.children << size_value
	size_field := ast.add_node(flat.Node{
		kind: .field_decl
		value: 'size'
		typ: 'int'
		children_start: size_field_start
		children_count: 1
	})
	struct_start := ast.children.len
	ast.children << x_field
	ast.children << items_field
	ast.children << channel_field
	ast.children << size_field
	mut box_decl := flat.Node{
		kind: .struct_decl
		value: 'GenericBox'
		children_start: struct_start
		children_count: 4
	}
	box_decl.set_generic_params(['T'])
	box_id := ast.add_node(box_decl)
	mut tc := types.TypeChecker.new(&ast)
	tc.structs['GenericBox[int]'] = [
		types.StructField{ name: 'x', typ: types.Type(types.int_), has_default: true },
		types.StructField{
			name: 'items'
			typ: types.Type(types.Array{ elem_type: types.Type(types.int_) })
		},
		types.StructField{
			name: 'ch'
			typ: types.Type(types.Channel{ elem_type: types.Type(types.int_) })
			has_default: true
		},
		types.StructField{ name: 'size', typ: types.Type(types.int_), has_default: true },
	]
	tc.structs['GenericBox_int'] = [
		types.StructField{ name: 'x', typ: types.Type(types.int_), has_default: true },
		types.StructField{
			name: 'items'
			typ: types.Type(types.Array{ elem_type: types.Type(types.int_) })
		},
		types.StructField{
			name: 'ch'
			typ: types.Type(types.Channel{ elem_type: types.Type(types.int_) })
			has_default: true
		},
		types.StructField{ name: 'size', typ: types.Type(types.int_), has_default: true },
	]
	tc.structs['GenericBox[string]'] = [
		types.StructField{ name: 'x', typ: types.Type(types.int_), has_default: true },
		types.StructField{
			name: 'items'
			typ: types.Type(types.Array{ elem_type: types.Type(types.string_) })
		},
		types.StructField{
			name: 'ch'
			typ: types.Type(types.Channel{ elem_type: types.Type(types.string_) })
			has_default: true
		},
		types.StructField{ name: 'size', typ: types.Type(types.int_), has_default: true },
	]
	tc.structs['GenericBox_string'] = [
		types.StructField{ name: 'x', typ: types.Type(types.int_), has_default: true },
		types.StructField{
			name: 'items'
			typ: types.Type(types.Array{ elem_type: types.Type(types.string_) })
		},
		types.StructField{
			name: 'ch'
			typ: types.Type(types.Channel{ elem_type: types.Type(types.string_) })
			has_default: true
		},
		types.StructField{ name: 'size', typ: types.Type(types.int_), has_default: true },
	]
	tc.structs['other.GenericBox[int]'] = [
		types.StructField{ name: 'x', typ: types.Type(types.int_), has_default: true },
		types.StructField{
			name: 'items'
			typ: types.Type(types.Array{ elem_type: types.Type(types.int_) })
		},
		types.StructField{
			name: 'ch'
			typ: types.Type(types.Channel{ elem_type: types.Type(types.int_) })
			has_default: true
		},
		types.StructField{ name: 'size', typ: types.Type(types.int_), has_default: true },
	]
	tc.structs['domainmain.GenericBox[int]'] = [
		types.StructField{ name: 'x', typ: types.Type(types.int_), has_default: true },
		types.StructField{
			name: 'items'
			typ: types.Type(types.Array{ elem_type: types.Type(types.int_) })
		},
		types.StructField{
			name: 'ch'
			typ: types.Type(types.Channel{ elem_type: types.Type(types.int_) })
			has_default: true
		},
		types.StructField{ name: 'size', typ: types.Type(types.int_), has_default: true },
	]
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.struct_decl_infos['GenericBox'] = StructDeclInfo{
		node: box_decl
		node_id: int(box_id)
		module: 'main'
		file: 'main.v'
		full_name: 'GenericBox'
	}
	g.struct_decl_short_infos['GenericBox'] = g.struct_decl_infos['GenericBox']

	default_value_text := g.default_value_to_string(types.Type(types.Struct{ name: 'GenericBox_int' }))
	main_default_value_text := g.default_value_to_string(types.Type(types.Struct{ name: 'main__GenericBox_int' }))
	string_default_value_text := g.default_value_to_string(types.Type(types.Struct{ name: 'GenericBox_string' }))

	assert default_value_text.contains('.x = 5')
	assert default_value_text.contains('.items = array_new')
	assert main_default_value_text.contains('.x = 5')
	assert main_default_value_text.contains('.items = array_new')
	tc.cur_module = 'other'
	assert g.generic_struct_application_for_flattened_name('GenericBox_int')? == 'GenericBox[int]'
	assert g.generic_struct_application_for_flattened_name('main__GenericBox_int')? == 'GenericBox[int]'
	tc.structs['main.GenericBox[int]'] = tc.structs['GenericBox[int]']
	assert g.generic_struct_application_for_flattened_name('GenericBox_int') == none
	assert string_default_value_text.contains('.ch = sync__new_channel_st((u32)(1), (u32)(sizeof(string)))')
	assert string_default_value_text.contains('.size = sizeof(string)')
}

fn test_generic_text_substitution_preserves_fn_field_type_params() {
	assert substitute_shared_generic_type_text('fn (T) u32', ['T'], ['int']) == 'fn (int) u32'
	mut_fn := substitute_shared_generic_type_text('fn (mut T, []T) ?T', ['T'], ['string'])
	assert mut_fn == 'fn (mut string, []string) ?string'
	nested_fn := substitute_shared_generic_type_text('fn (next fn (T) T) T', ['T'], ['int'])
	assert nested_fn == 'fn (next fn (int) int) int'
	assert shared_type_text_uses_generic_params('fn (mut it T) T', ['T'])
	assert shared_fn_param_type_text('mut it &Dog') == '&Dog'
}

fn test_struct_default_generic_args_preserve_caller_module() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.cur_module = 'main'
	tc.cur_file = 'main.v'
	tc.structs['Local'] = []types.StructField{}
	tc.structs['lib.Local'] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.register_struct_decl_info('Local', 'Local', 'main', 'main.v', flat.Node{
		kind: .struct_decl
		value: 'Local'
	})
	g.register_struct_decl_info('Local', 'lib.Local', 'lib', 'lib.v', flat.Node{
		kind: .struct_decl
		value: 'Local'
	})

	args := g.struct_default_canonical_generic_args(['Local', '&Local', 'fn (Local) Local'])
	assert args == ['main.Local', '&main.Local', 'fn (main.Local) main.Local']
	resolved := g.struct_default_field_type_text(g.struct_decl_infos['lib.Local'], args[0])
	assert resolved.name() == 'main.Local'
}

fn test_promoted_root_declared_default_recovers_generic_source() {
	mut ast := flat.FlatAst.new()
	size_value := ast.add_node(flat.Node{
		kind: .sizeof_expr
		value: 'T'
	})
	a_start := ast.children.len
	ast.children << size_value
	a_field := ast.add_node(flat.Node{
		kind: .field_init
		value: 'a'
		children_start: a_start
		children_count: 1
	})
	b_value := ast.add_node(flat.Node{
		kind: .int_literal
		value: '4'
	})
	b_start := ast.children.len
	ast.children << b_value
	b_field := ast.add_node(flat.Node{
		kind: .field_init
		value: 'b'
		children_start: b_start
		children_count: 1
	})
	init_start := ast.children.len
	ast.children << a_field
	ast.children << b_field
	inner_init := ast.add_node(flat.Node{
		kind: .struct_init
		value: 'Inner'
		typ: 'Inner'
		children_start: init_start
		children_count: 2
	})
	embed_start := ast.children.len
	ast.children << inner_init
	embed_field := ast.add_node(flat.Node{
		kind: .field_decl
		value: 'Inner'
		children_start: embed_start
		children_count: 1
	})
	struct_start := ast.children.len
	ast.children << embed_field
	mut outer_decl := flat.Node{
		kind: .struct_decl
		value: 'Outer'
		children_start: struct_start
		children_count: 1
	}
	outer_decl.set_generic_params(['T'])
	outer_id := ast.add_node(outer_decl)
	mut tc := types.TypeChecker.new(&ast)
	tc.structs['Inner'] = [
		types.StructField{ name: 'a', typ: types.Type(types.int_) },
		types.StructField{ name: 'b', typ: types.Type(types.int_) },
	]
	tc.structs['Outer[i64]'] = [
		types.StructField{
			name: 'Inner'
			typ: types.Type(types.Struct{ name: 'Inner' })
			is_embed: true
			has_default: true
		},
	]
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.struct_decl_infos['Outer'] = StructDeclInfo{
		node: outer_decl
		node_id: int(outer_id)
		module: 'main'
		file: 'main.v'
		full_name: 'Outer'
	}
	g.struct_decl_short_infos['Outer'] = g.struct_decl_infos['Outer']

	// Outer[i64]{ b: 7 } promotes b into the embedded root. Only a is still
	// owed by the root's declared initializer.
	mut set := map[string]bool{}
	set['Inner.b'] = true
	assert g.gen_promoted_root_declared_default('Outer_i64', 'Inner', 'Inner', 'Inner', mut set, false)
	out := g.sb.str()
	assert out == '.Inner.a = sizeof(i64)'
	assert g.struct_default_generic_params.len == 0
	assert g.struct_default_generic_args.len == 0
}
