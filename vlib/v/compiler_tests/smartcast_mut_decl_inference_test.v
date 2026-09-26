type SmartcastDeclValue = int | string

fn smartcast_decl_reassign_after_guard(value SmartcastDeclValue) SmartcastDeclValue {
	if value !is int {
		return value
	}
	mut copy := value
	copy = 'changed'
	return copy
}

fn test_mut_declaration_from_guard_smartcast_keeps_original_sum_type() {
	assert smartcast_decl_reassign_after_guard(SmartcastDeclValue(1)) == SmartcastDeclValue('changed')
}

fn smartcast_decl_reassign_after_assert(value SmartcastDeclValue) SmartcastDeclValue {
	assert value is int
	mut copy := value
	copy = 'changed'
	return copy
}

fn test_mut_declaration_from_assert_smartcast_keeps_original_sum_type() {
	assert smartcast_decl_reassign_after_assert(SmartcastDeclValue(1)) == SmartcastDeclValue('changed')
}

fn test_mut_declaration_from_loop_smartcast_is_narrowed() {
	value := SmartcastDeclValue(1)
	for value is int {
		mut copy := value
		copy = 2
		assert copy == 2
		break
	}
}
