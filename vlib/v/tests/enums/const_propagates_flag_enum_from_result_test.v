@[flag]
pub enum TransFlags {
	no_deps
	no_save
	recurse
}

const trans_flags_none = TransFlags.from(0)!
const trans_flags_combined = TransFlags.from(5)!

fn propagate_trans_flags(value int) !TransFlags {
	return TransFlags.from(value)!
}

@[flag]
enum CustomTransFlags {
	first
}

fn CustomTransFlags.from(value int) !int {
	return value + 40
}

fn test_const_flag_enum_from_result_preserves_values() {
	assert int(trans_flags_none) == 0
	assert trans_flags_combined == TransFlags.no_deps | .recurse
	assert propagate_trans_flags(2)! == .no_save
}

fn test_flag_enum_from_propagates_error() {
	propagate_trans_flags(8) or {
		assert err.msg() == 'invalid value'
		return
	}
	assert false
}

fn test_custom_flag_enum_from_keeps_its_result_type() {
	assert CustomTransFlags.from(2)! == 42
}
