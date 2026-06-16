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

fn test_sumtype_match_alias_matches_original_variant_tag() {
	v := AliasTagSumWithAlias(AliasTagNull{})
	match v {
		AliasTagNullAlias {
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

fn test_sumtype_match_original_matches_alias_variant_tag() {
	v := AliasTagSumWithAlias(AliasTagNullAlias{})
	match v {
		AliasTagNull {
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
		AliasTagNull, int {
			return v is AliasTagNullAlias
		}
		else {
			return false
		}
	}
}

fn test_sumtype_is_in_aggregate_branch_matches_alias_tags() {
	assert aggregate_branch_is_original(AliasTagSumWithAlias(AliasTagNullAlias{}))
	assert aggregate_branch_is_alias(AliasTagSumWithAlias(AliasTagNullAlias{}))
	assert !aggregate_branch_is_original(AliasTagSumWithAlias(1))
	assert !aggregate_branch_is_alias(AliasTagSumWithAlias(1))
}

struct AliasTagOptionalPayload {}

type AliasTagOptionalPayloadAlias = ?AliasTagOptionalPayload
type AliasTagOptionalSum = ?AliasTagOptionalPayload | int

fn alias_tag_get_optional_payload() AliasTagOptionalPayloadAlias {
	return AliasTagOptionalPayload{}
}

fn test_sumtype_is_matches_alias_to_option_parent_runtime_tag() {
	box := AliasTagOptionalSum(alias_tag_get_optional_payload())
	assert box is AliasTagOptionalPayloadAlias
}

fn test_sumtype_match_matches_alias_to_option_parent_runtime_tag() {
	box := AliasTagOptionalSum(alias_tag_get_optional_payload())
	match box {
		AliasTagOptionalPayloadAlias {
			assert true
		}
		else {
			assert false
		}
	}
}

struct AliasTagAsPayload {
	value int
}

type AliasTagAsPayloadOption = ?AliasTagAsPayload
type AliasTagAsOptionSum = ?AliasTagAsPayload | int

fn alias_tag_as_payload(value int) AliasTagAsPayloadOption {
	return AliasTagAsPayload{
		value: value
	}
}

fn test_sumtype_as_option_parent_alias_keeps_option_payload_field() {
	box := AliasTagAsOptionSum(alias_tag_as_payload(17))
	got := box as AliasTagAsPayloadOption
	assert got != none
}

fn test_sumtype_as_option_parent_keeps_option_payload_field() {
	box := AliasTagAsOptionSum(alias_tag_as_payload(23))
	got := box as ?AliasTagAsPayload
	if value := got {
		assert value.value == 23
	} else {
		assert false
	}
}
