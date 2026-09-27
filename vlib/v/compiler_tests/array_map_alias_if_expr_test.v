type AliasIndex = u32

fn (index AliasIndex) is_wrapped() bool {
	return index > 10
}

fn (index AliasIndex) unwrap() AliasIndex {
	return index - 10
}

fn test_array_map_alias_if_expr_keeps_alias_type() {
	values := [AliasIndex(1), AliasIndex(12)]
	unwrapped := values.map(if it.is_wrapped() { it.unwrap() } else { it })
	assert unwrapped == [AliasIndex(1), AliasIndex(2)]
}

struct AliasIndexes {
mut:
	values []AliasIndex
}

fn test_array_map_alias_if_expr_from_mutable_pointer_field() {
	mut indexes := &AliasIndexes{
		values: [AliasIndex(1), AliasIndex(12)]
	}
	indexes.values = indexes.values.map(if it.is_wrapped() { it.unwrap() } else { it })
	assert indexes.values == [AliasIndex(1), AliasIndex(2)]
}
