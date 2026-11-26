import v.tests.enums.enumfromfirst
import v.tests.enums.enumfromsecond

enum Choice {
	local = 12
}

fn test_enum_from_uses_the_declaring_module() {
	assert enumfromfirst.parse(1) == 1
	assert enumfromfirst.parse(2) == -1
	assert enumfromsecond.parse(2) == 2
	assert enumfromsecond.parse(1) == -1
	assert Choice.from(12)! == .local
}

fn enum_from_input(succeeds bool) !int {
	if !succeeds {
		return error('input failed')
	}
	return 12
}

fn enum_from_match_with_handler(use_result bool, succeeds bool) !Choice {
	return Choice.from(match use_result {
		true { enum_from_input(succeeds)! }
		else { 12 }
	}) or { return err }
}

fn enum_from_match_as_result(use_result bool, succeeds bool) !Choice {
	return Choice.from(match use_result {
		true { enum_from_input(succeeds)! }
		else { 12 }
	})
}

fn test_enum_from_preserves_a_propagating_match_value() {
	assert enum_from_match_with_handler(true, true)! == .local
	assert enum_from_match_with_handler(false, false)! == .local
	if _ := enum_from_match_with_handler(true, false) {
		assert false
	} else {
		assert err.msg() == 'input failed'
	}
	assert enum_from_match_as_result(true, true)! == .local
	if _ := enum_from_match_as_result(true, false) {
		assert false
	} else {
		assert err.msg() == 'input failed'
	}
}
