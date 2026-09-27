module transform

import v.flat
import v.types

fn test_specialized_signature_preserves_variadic_call_convention() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	decl := GenericFnDecl{ module: 'main', file: 'main.v' }
	assert t.specialized_signature_type_text(decl, '...int', ['f64'], ['T']) == '...int'
	assert t.specialized_signature_type_text(decl, '...T', ['f64'], ['T']) == '...f64'
	assert t.specialized_signature_type_text(decl, '[]int', ['f64'], ['T']) == '[]int'
	assert t.specialized_signature_type_text(decl, '[]T', ['f64'], ['T']) == '[]f64'
}

fn test_specialized_variadic_element_keeps_declaration_scope() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['gates.Param'] = []types.StructField{}
	tc.struct_modules['gates.Param'] = 'gates'
	tc.structs['Param'] = []types.StructField{}
	tc.struct_modules['Param'] = 'main'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	decl := GenericFnDecl{ module: 'gates', file: 'gates/gates.v' }
	assert t.specialized_signature_type_text(decl, '...Param', ['int'], ['T']) == '...gates.Param'
	assert t.specialized_signature_type_text(decl, '...&Param', ['int'], ['T']) == '...&gates.Param'
}
