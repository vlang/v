@[has_globals]
module main

__global (
	alias_tag_side_effect_calls int
)

interface AliasTagValue {}

struct AliasTagNull {}

struct AliasTagOther {}

type AliasTagNullAlias = AliasTagNull

fn alias_tag_get_null() AliasTagValue {
	return AliasTagValue(AliasTagNull{})
}

fn alias_tag_unwrap_null(v AliasTagValue) AliasTagNull {
	match v {
		AliasTagNull {
			return v
		}
		else {
			panic('expected AliasTagNull')
		}
	}
}

fn test_interface_is_matches_alias_to_implementor() {
	v := alias_tag_get_null()
	assert v is AliasTagNullAlias
}

fn alias_tag_get_other_with_side_effect() AliasTagValue {
	unsafe {
		alias_tag_side_effect_calls++
	}
	return AliasTagValue(AliasTagOther{})
}

fn alias_tag_zero_index_with_side_effect() int {
	unsafe {
		alias_tag_side_effect_calls++
	}
	return 0
}

fn test_interface_is_alias_operand_is_evaluated_once() {
	unsafe {
		alias_tag_side_effect_calls = 0
	}
	assert alias_tag_get_other_with_side_effect() !is AliasTagNullAlias
	assert alias_tag_side_effect_calls == 1
}

fn test_interface_is_alias_indexed_lvalue_is_evaluated_once() {
	unsafe {
		alias_tag_side_effect_calls = 0
	}
	values := [AliasTagValue(AliasTagOther{})]
	assert values[alias_tag_zero_index_with_side_effect()] !is AliasTagNullAlias
	assert alias_tag_side_effect_calls == 1
}

fn test_interface_match_matches_alias_boxed_value() {
	v := AliasTagNullAlias{}
	assert alias_tag_unwrap_null(v) == AliasTagNull{}
}

fn test_interface_as_original_matches_alias_boxed_value() {
	v := AliasTagValue(AliasTagNullAlias{})
	got := v as AliasTagNull
	assert got == AliasTagNull{}
}

fn test_interface_as_alias_matches_original_boxed_value() {
	v := AliasTagValue(AliasTagNull{})
	got := v as AliasTagNullAlias
	assert got == AliasTagNullAlias{}
}

interface AliasTagSingleValue {}

struct AliasTagSingleNull {}

type AliasTagSingleNullAlias = AliasTagSingleNull

fn test_interface_as_alias_matches_single_original_boxed_value() {
	v := AliasTagSingleValue(AliasTagSingleNull{})
	assert v is AliasTagSingleNullAlias
	got := v as AliasTagSingleNullAlias
	assert got == AliasTagSingleNullAlias{}
}

type AliasTagSum = AliasTagNull | int
type AliasTagSumWithAlias = AliasTagNull | AliasTagNullAlias | int

fn alias_tag_get_sum_null() AliasTagSum {
	return AliasTagNull{}
}

fn test_sumtype_is_matches_alias_to_variant() {
	v := alias_tag_get_sum_null()
	assert v is AliasTagNullAlias
}

fn test_sumtype_match_matches_alias_boxed_value() {
	v := AliasTagSum(AliasTagNullAlias{})
	match v {
		AliasTagNull {
			assert true
		}
		else {
			assert false
		}
	}
}

fn test_sumtype_as_alias_matches_single_original_variant_tag() {
	v := AliasTagSum(AliasTagNull{})
	assert v is AliasTagNullAlias
	got := v as AliasTagNullAlias
	assert got == AliasTagNullAlias{}
}

fn test_sumtype_is_alias_matches_original_variant_tag() {
	v := AliasTagSumWithAlias(AliasTagNull{})
	assert v is AliasTagNullAlias
}

fn test_sumtype_match_alias_branch_prefers_alias_variant_tag_when_alias_is_variant() {
	v := AliasTagSumWithAlias(AliasTagNull{})
	match v {
		AliasTagNullAlias {
			assert false
		}
		AliasTagNull {
			assert true
		}
		else {
			assert false
		}
	}
}

fn test_sumtype_is_original_matches_alias_variant_tag() {
	v := AliasTagSumWithAlias(AliasTagNullAlias{})
	assert v is AliasTagNull
}

fn test_sumtype_match_original_branch_prefers_original_variant_tag_when_alias_is_variant() {
	v := AliasTagSumWithAlias(AliasTagNullAlias{})
	match v {
		AliasTagNull {
			assert false
		}
		AliasTagNullAlias {
			assert true
		}
		else {
			assert false
		}
	}
}

fn test_sumtype_as_original_matches_alias_variant_tag() {
	v := AliasTagSumWithAlias(AliasTagNullAlias{})
	got := v as AliasTagNull
	assert got == AliasTagNull{}
}

fn test_sumtype_as_alias_matches_original_variant_tag() {
	v := AliasTagSumWithAlias(AliasTagNull{})
	got := v as AliasTagNullAlias
	assert got == AliasTagNullAlias{}
}

fn aggregate_branch_is_original(v AliasTagSumWithAlias) bool {
	match v {
		AliasTagNull, int {
			return v is AliasTagNull
		}
		else {
			return false
		}
	}
}

fn aggregate_branch_is_alias(v AliasTagSumWithAlias) bool {
	match v {
		AliasTagNullAlias, int {
			return v is AliasTagNullAlias
		}
		else {
			return false
		}
	}
}

fn test_sumtype_match_aggregate_branch_uses_exact_alias_tags() {
	assert aggregate_branch_is_original(AliasTagSumWithAlias(AliasTagNull{}))
	assert !aggregate_branch_is_alias(AliasTagSumWithAlias(AliasTagNull{}))
	assert !aggregate_branch_is_original(AliasTagSumWithAlias(AliasTagNullAlias{}))
	assert aggregate_branch_is_alias(AliasTagSumWithAlias(AliasTagNullAlias{}))
	assert !aggregate_branch_is_original(AliasTagSumWithAlias(1))
	assert !aggregate_branch_is_alias(AliasTagSumWithAlias(1))
}

type Issue27718RawOffset = i8
type Issue27718SPOffset = Issue27718RawOffset
type Issue27718Offset = Issue27718RawOffset | Issue27718SPOffset

fn (o Issue27718Offset) str() string {
	return match o {
		Issue27718SPOffset {
			sign := if i8(o) < 0 { '' } else { '+' }
			'SP${sign}${Issue27718RawOffset(o)}'
		}
		Issue27718RawOffset {
			o.str()
		}
	}
}

fn test_sumtype_match_uses_exact_alias_variant_tag_for_chained_aliases() {
	assert Issue27718Offset(Issue27718SPOffset(42)).str() == 'SP+42'
	assert Issue27718Offset(Issue27718RawOffset(42)).str() == '42'
}

type Issue27719RawMode = u8
type Issue27719SpecialMode = Issue27719RawMode
type Issue27719Mode = Issue27719RawMode | Issue27719SpecialMode

fn issue27719_mode_name(mode Issue27719Mode) string {
	return match mode {
		Issue27719SpecialMode { 'special' }
		Issue27719RawMode { 'raw' }
	}
}

fn test_sumtype_match_expr_uses_exact_alias_variant_tag_for_chained_aliases() {
	assert issue27719_mode_name(Issue27719Mode(Issue27719SpecialMode(1))) == 'special'
	assert issue27719_mode_name(Issue27719Mode(Issue27719RawMode(1))) == 'raw'
}
