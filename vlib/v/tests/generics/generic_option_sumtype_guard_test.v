struct Named {
	name string
}

struct Empty {}

type Reference = Empty | Named

fn unwrap[T](value ?T) T {
	return value or { panic('none') }
}

fn name_after_guard(value ?Reference) string {
	if value == none {
		return 'none'
	}
	unwrapped := unwrap(value)
	if unwrapped is Named {
		return unwrapped.name
	}
	return 'empty'
}

fn test_guarded_optional_sumtype_is_passed_to_generic_option_parameter() {
	assert name_after_guard(Reference(Named{ name: 'value' })) == 'value'
	assert name_after_guard(Reference(Empty{})) == 'empty'
	assert name_after_guard(none) == 'none'
}
