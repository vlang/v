type GenericSumtypeAliasValue[T] = string | int | T

fn generic_sumtype_alias_accepts_int_literal[T](value GenericSumtypeAliasValue[T]) int {
	_ = value
	return 0
}

fn test_generic_sumtype_alias_accepts_int_literal() {
	assert generic_sumtype_alias_accepts_int_literal(5) == 0
}
