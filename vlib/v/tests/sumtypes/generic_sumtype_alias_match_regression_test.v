struct GenericSumtypeAliasMatchItem {}

type GenericSumtypeAliasMatchValue[T] = string | int | T

fn generic_sumtype_alias_match_exhaustive[T](value GenericSumtypeAliasMatchValue[T]) int {
	return match value {
		string { 1 }
		int { 2 }
		T { 3 }
	}
}

fn test_generic_sumtype_alias_match_with_generic_variant_is_exhaustive() {
	assert generic_sumtype_alias_match_exhaustive(GenericSumtypeAliasMatchItem{}) == 3
}

type GenericSumtypeAliasNestedInner = int | string
type GenericSumtypeAliasNestedOuter[T] = GenericSumtypeAliasNestedInner | T

fn generic_sumtype_alias_match_nested_variants[T](value GenericSumtypeAliasNestedOuter[T]) int {
	return match value {
		int { 1 }
		string { 2 }
		T { 3 }
	}
}

fn test_generic_sumtype_alias_match_expands_nested_sumtype_variants() {
	assert generic_sumtype_alias_match_nested_variants[bool](1) == 1
	assert generic_sumtype_alias_match_nested_variants[bool](true) == 3
}

type GenericSumtypeAliasFnBox[T] = fn (int) int | T

fn generic_sumtype_alias_match_increment(value int) int {
	return value + 1
}

fn generic_sumtype_alias_match_fn_variant[T](value GenericSumtypeAliasFnBox[T]) int {
	return match value {
		fn (int) int { value(41) }
		T { 0 }
	}
}

fn test_generic_sumtype_alias_match_function_variant_by_signature() {
	assert generic_sumtype_alias_match_fn_variant[string](generic_sumtype_alias_match_increment) == 42
}
