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
