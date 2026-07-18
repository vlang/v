type AorB = u8 | u16

type MaybeAorB = ?AorB

struct OptionSumTypeHolder {
	value ?AorB
}

fn test_cast_to_option_alias_of_sum_type() {
	alias := ?MaybeAorB(u8(1))
	assert '${alias}' == 'Option(AorB(1))'
}

fn test_typeof_option_sum_type_field() {
	assert unsafe { typeof(OptionSumTypeHolder{}.value) } == '?AorB'
}
