type AliasTagMarker = AliasTagString | AliasTagVoid | AliasTagRune

type AliasTagString = u8
type AliasTagVoid = u8
type AliasTagRune = u8
type AliasTagVoidRef = AliasTagVoid

fn test_sumtype_is_keeps_distinct_alias_variants_with_same_parent_type() {
	string_marker := AliasTagMarker(AliasTagString(0))
	void_marker := AliasTagMarker(AliasTagVoid(0))
	rune_marker := AliasTagMarker(AliasTagRune(0))

	assert string_marker is AliasTagString
	assert string_marker !is AliasTagVoid
	assert string_marker !is AliasTagRune
	assert void_marker is AliasTagVoid
	assert void_marker !is AliasTagString
	assert rune_marker is AliasTagRune
	assert rune_marker !is AliasTagString
}

fn test_sumtype_wrap_keeps_alias_variant_lineage_with_same_parent_type() {
	void_ref_marker := AliasTagMarker(AliasTagVoidRef(0))

	assert void_ref_marker is AliasTagVoid
	assert void_ref_marker !is AliasTagString
}
