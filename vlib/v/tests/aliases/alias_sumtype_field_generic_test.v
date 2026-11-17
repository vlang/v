import alias_sumtype_is_module

type GenericFieldSum[T] = T | string
type GenericFieldAlias = GenericFieldSum[int]
type GenericFieldAliasTwo = GenericFieldAlias
type GenericFieldAliasThree = GenericFieldAliasTwo

struct GenericAliasBox {
	value GenericFieldAlias
}

struct GenericAliasDeepBox {
	value GenericFieldAliasThree
}

fn generic_alias_field_kind(b GenericAliasBox) string {
	return match b.value {
		int { 'int' }
		string { 'string' }
	}
}

fn generic_alias_field_is_int(b GenericAliasBox) bool {
	return b.value is int
}

fn generic_alias_field_is_not_int(b GenericAliasBox) bool {
	return b.value !is int
}

fn test_generic_sum_alias_field_keeps_concrete_arguments() {
	integer := GenericAliasBox{ value: GenericFieldAlias(GenericFieldSum[int](7)) }
	text := GenericAliasBox{ value: GenericFieldAlias(GenericFieldSum[int]('text')) }
	assert generic_alias_field_is_int(integer)
	assert !generic_alias_field_is_not_int(integer)
	assert !generic_alias_field_is_int(text)
	assert generic_alias_field_is_not_int(text)
}

fn test_generic_sum_alias_field_match_keeps_concrete_arguments() {
	assert generic_alias_field_kind(GenericAliasBox{
		value: GenericFieldAlias(GenericFieldSum[int](7))
	}) == 'int'
	assert generic_alias_field_kind(GenericAliasBox{
		value: GenericFieldAlias(GenericFieldSum[int]('text'))
	}) == 'string'
}

fn generic_alias_deep_field_kind(b GenericAliasDeepBox) string {
	return match b.value {
		int { 'int' }
		string { 'string' }
	}
}

fn generic_alias_deep_field_is_int(b GenericAliasDeepBox) bool {
	return b.value is int
}

fn generic_alias_deep_field_is_not_int(b GenericAliasDeepBox) bool {
	return b.value !is int
}

fn test_generic_sum_alias_field_keeps_arguments_through_alias_chain() {
	integer := GenericAliasDeepBox{
		value: GenericFieldAliasThree(GenericFieldAliasTwo(GenericFieldAlias(GenericFieldSum[int](7))))
	}
	text := GenericAliasDeepBox{
		value: GenericFieldAliasThree(GenericFieldAliasTwo(GenericFieldAlias(GenericFieldSum[int]('text'))))
	}
	assert generic_alias_deep_field_is_int(integer)
	assert !generic_alias_deep_field_is_not_int(integer)
	assert !generic_alias_deep_field_is_int(text)
	assert generic_alias_deep_field_is_not_int(text)
	assert generic_alias_deep_field_kind(integer) == 'int'
	assert generic_alias_deep_field_kind(text) == 'string'
}

type AliasIsScopeValue = int | string

fn test_sum_is_in_imported_generic_preserves_main_type_identity() {
	integer := AliasIsScopeValue(7)
	text := AliasIsScopeValue('text')
	assert alias_sumtype_is_module.is_int[AliasIsScopeValue](integer)
	assert !alias_sumtype_is_module.is_int[AliasIsScopeValue](text)
	assert !alias_sumtype_is_module.is_not_int[AliasIsScopeValue](integer)
	assert alias_sumtype_is_module.is_not_int[AliasIsScopeValue](text)
	assert alias_sumtype_is_module.kind[AliasIsScopeValue](integer) == 'int'
	assert alias_sumtype_is_module.kind[AliasIsScopeValue](text) == 'string'
}
