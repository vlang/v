@[has_globals]
module main

struct GlobalOptionalGuardHolder {
	value ?string
}

__global global_optional_guard_holder = GlobalOptionalGuardHolder{ value: '  hello  ' }

const const_optional_guard_holder = GlobalOptionalGuardHolder{ value: '  constant  ' }

struct GlobalOptionalArrayHolder {
	values ?[2]string
}

const const_optional_array_holder = GlobalOptionalArrayHolder{ values: ['first', 'second']! }

fn global_optional_guard() !string {
	if global_optional_guard_holder.value == none {
		return error('value is required')
	}
	if value := global_optional_guard_holder.value {
		return value.trim_space()
	}
	return error('value is required')
}

fn global_optional_guard_expr() !string {
	if global_optional_guard_holder.value == none {
		return error('value is required')
	}
	return if value := global_optional_guard_holder.value { value.trim_space() } else { '' }
}

fn test_global_optional_field_guards_after_none_check() {
	assert global_optional_guard()! == 'hello'
	assert global_optional_guard_expr()! == 'hello'
}

fn const_optional_guard() !string {
	if const_optional_guard_holder.value == none {
		return error('value is required')
	}
	return if value := const_optional_guard_holder.value { value.trim_space() } else { '' }
}

fn test_const_optional_field_guard_after_none_check() {
	assert const_optional_guard()! == 'constant'
}

fn test_const_optional_fixed_array_field() {
	if values := const_optional_array_holder.values {
		assert values == ['first', 'second']!
	} else {
		assert false
	}
}
