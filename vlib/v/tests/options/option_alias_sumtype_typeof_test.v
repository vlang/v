type AorB = u8 | u16

type MaybeAorB = ?AorB

type PayloadAlias = int

type MaybePayloadAlias = ?PayloadAlias

struct OptionSumTypeHolder {
	value ?AorB
}

fn (value PayloadAlias) str() string {
	return 'PayloadAlias(${int(value)})'
}

fn test_cast_to_option_alias_of_sum_type() {
	alias := ?MaybeAorB(u8(1))
	assert '${alias}' == 'Option(AorB(1))'
}

fn test_typeof_option_sum_type_field() {
	assert unsafe { typeof(OptionSumTypeHolder{}.value) } == '?AorB'
}

fn test_option_alias_preserves_aliased_payload_str() {
	alias := ?MaybePayloadAlias(PayloadAlias(1))
	assert '${alias}' == 'Option(PayloadAlias(1))'
}
