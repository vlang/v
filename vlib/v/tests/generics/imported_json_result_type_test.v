import imported_json_result_type

struct Token {
	id string
}

type TokenAlias = Token

fn test_imported_json_result_keeps_its_declaring_module() {
	local := Token{ id: 'local' }
	assert local.id == 'local'
	token := imported_json_result_type.token('{"id":7}')!
	assert token.id == 7
	propagated := imported_json_result_type.propagated_token('{"id":11}')!
	assert propagated.id == 11
	assert imported_json_result_type.fallback_token('{"id":13}').id == 13
	assert imported_json_result_type.fallback_token('invalid json').id == 29
}

fn test_imported_json_result_propagates_errors() {
	if _ := imported_json_result_type.token('invalid json') {
		assert false, 'decoding should fail'
	} else {
		assert err.msg().len > 0
	}
	if _ := imported_json_result_type.propagated_token('invalid json') {
		assert false, 'decoding should fail'
	} else {
		assert err.msg().len > 0
	}
}

fn test_imported_result_keeps_alias_and_pointer_types() {
	local := TokenAlias(Token{ id: 'alias' })
	assert local.id == 'alias'
	aliased := imported_json_result_type.aliased_token()!
	assert aliased.id == 31
	assert typeof(aliased).name == typeof[imported_json_result_type.TokenAlias]().name
	pointer := imported_json_result_type.token_pointer()!
	assert pointer.id == 37
	assert typeof(pointer).name == typeof[&imported_json_result_type.Token]().name
}

fn test_generic_result_array_in_if_expression() {
	populated := imported_json_result_type.entries(true)!
	assert populated.len == 2
	assert populated[0].id == 1
	assert populated[1].id == 2
	empty := imported_json_result_type.entries(false)!
	assert empty.len == 1
	assert empty[0].id == 2
}
