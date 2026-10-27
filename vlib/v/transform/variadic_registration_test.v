module transform

import v.flat
import v.types

fn test_specialized_signature_preserves_variadic_call_convention() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	decl := GenericFnDecl{ module: 'main', file: 'main.v' }
	assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('...int'), flat.TypeTextList.from_names(['f64']).ids, ['T'])) == '...int'
	assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('...T'), flat.TypeTextList.from_names(['f64']).ids, ['T'])) == '...f64'
	assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('[]int'), flat.TypeTextList.from_names(['f64']).ids, ['T'])) == '[]int'
	assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('[]T'), flat.TypeTextList.from_names(['f64']).ids, ['T'])) == '[]f64'
}

fn test_specialized_variadic_element_keeps_declaration_scope() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('gates.Param')] = []types.StructField{}
	tc.struct_modules[flat.source_type_id('gates.Param')] = 'gates'
	tc.structs[flat.source_type_id('Param')] = []types.StructField{}
	tc.struct_modules[flat.source_type_id('Param')] = 'main'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	decl := GenericFnDecl{ module: 'gates', file: 'gates/gates.v' }
	assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('...Param'), flat.TypeTextList.from_names(['int']).ids, ['T'])) == '...gates.Param'
	assert flat.source_type_name(t.specialized_signature_type_text(decl, flat.source_type_id('...&Param'), flat.TypeTextList.from_names(['int']).ids, ['T'])) == '...&gates.Param'
}
