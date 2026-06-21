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
