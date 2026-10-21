module c

import v.flat
import v.types

fn test_optional_pointer_aliases_forward_the_underlying_struct() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	pointee := types.Type(types.Struct{ name_id: flat.source_type_id('payload.Missing') })
	pointer_alias := types.Type(types.Alias{
		name_id:   flat.source_type_id('PointerAlias')
		base_type: types.Type(types.Pointer{ base_type: pointee })
	})
	struct_alias := types.Type(types.Alias{ name_id: flat.source_type_id('RecordAlias'), base_type: pointee })
	g.optional_type_name(types.Type(types.ResultType{ base_type: pointer_alias }))
	g.optional_type_name(types.Type(types.OptionType{
		base_type: types.Type(types.Pointer{ base_type: struct_alias })
	}))
	g.optional_type_name(types.Type(types.ResultType{
		base_type: types.Type(types.Pointer{ base_type: pointer_alias })
	}))
	g.optional_typedefs()
	emitted := g.sb.str()
	assert emitted.contains('typedef struct payload__Missing payload__Missing;'), emitted
	assert emitted.contains('payload__Missing* value;'), emitted
	assert emitted.contains('payload__Missing** value;'), emitted
	assert !emitted.contains('typedef struct PointerAlias'), emitted
	assert !emitted.contains('typedef struct RecordAlias'), emitted
}

fn test_pruned_cached_pointer_wrappers_do_not_emit_unused_pointees() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	for name in ['Used', 'Unused'] {
		tc.structs['payload.${name}'] = []types.StructField{}
		tc.const_types['payload.${name.to_lower()}'] = *tc.parse_type(flat.source_type_id('!&payload.${name}'))
	}
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.collect_checker_declaration_signature_types()
	g.cache_decl_demand = true
	g.cache_decl_refs[g.const_ident_c_name('payload.used')] = true
	g.finish_cache_declaration_demand('')
	g.optional_typedefs()
	emitted := g.sb.str()
	assert emitted.contains('payload__Used* value;'), emitted
	assert !emitted.contains('payload__Unused'), emitted
}

fn test_thread_pointer_wrappers_keep_the_existing_runtime_typedef() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	for name in ['thread', 'thread int'] {
		pointee := types.Type(types.Struct{ name_id: flat.source_type_id(name) })
		pointer_alias := types.Type(types.Alias{
			name_id:   flat.source_type_id('ThreadPointer')
			base_type: types.Type(types.Pointer{ base_type: pointee })
		})
		g.optional_type_name(types.Type(types.ResultType{ base_type: pointer_alias }))
		g.optional_type_name(types.Type(types.OptionType{
			base_type: types.Type(types.Pointer{ base_type: pointee })
		}))
	}
	g.optional_typedefs()
	emitted := g.sb.str()
	assert emitted.contains('__v_thread* value;'), emitted
	assert !emitted.contains('typedef struct __v_thread __v_thread;'), emitted
}

fn test_optional_pointer_union_aliases_forward_the_union_tag() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.unions['payload.Missing'] = true
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	pointee := types.Type(types.Struct{ name_id: flat.source_type_id('payload.Missing') })
	alias := types.Type(types.Alias{
		name_id:   flat.source_type_id('UnionPointer')
		base_type: types.Type(types.Pointer{ base_type: pointee })
	})
	g.optional_type_name(types.Type(types.ResultType{ base_type: alias }))
	g.optional_type_name(types.Type(types.OptionType{
		base_type: types.Type(types.Pointer{ base_type: pointee })
	}))
	g.optional_typedefs()
	emitted := g.sb.str()
	declaration := 'typedef union payload__Missing payload__Missing;'
	assert emitted.count(declaration) == 1, emitted
	assert emitted.index(declaration)? < emitted.index('payload__Missing* value;')?, emitted
	assert !emitted.contains('typedef struct payload__Missing'), emitted
	assert !emitted.contains('typedef union UnionPointer'), emitted
}
