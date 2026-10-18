struct GuardSmartcastText {
	text string
}

struct GuardSmartcastNumber {
	number int
}

type GuardSmartcastValue = GuardSmartcastNumber | GuardSmartcastText

fn guard_smartcast_kind(value GuardSmartcastValue) string {
	return match value {
		GuardSmartcastText { 'text' }
		GuardSmartcastNumber { 'number' }
	}
}

fn guard_smartcast_option(present bool) ?string {
	if present {
		return 'payload'
	}
	return none
}

fn guard_smartcast_result(present bool) !string {
	if present {
		return 'payload'
	}
	return error('missing')
}

fn guard_smartcast_pair(value string) (string, string) {
	return value, 'selected'
}

fn guard_smartcast_text(value GuardSmartcastText) string {
	return value.text
}

fn (value GuardSmartcastText) read_text() string {
	return value.text
}

fn test_multi_value_map_guard_preserves_sibling_smartcasts() {
	values := {
		'present': 'payload'
	}
	for present in [false, true] {
		key := if present { 'present' } else { 'missing' }
		mut current := GuardSmartcastValue(GuardSmartcastText{'narrowed'})
		kept := GuardSmartcastValue(GuardSmartcastText{'kept'})
		replacement := GuardSmartcastValue(GuardSmartcastNumber{17})
		if mut current is GuardSmartcastText {
			if kept is GuardSmartcastText {
				first, second := if value := values[key] {
					current = replacement
					guard_smartcast_pair(value)
				} else {
					guard_smartcast_text(current), kept.text
				}
				assert guard_smartcast_text(kept) == 'kept'
				if present {
					assert first == 'payload' && second == 'selected'
					assert guard_smartcast_kind(current) == 'number'
				} else {
					assert first == 'narrowed' && second == 'kept'
					assert guard_smartcast_kind(current) == 'text'
				}
			}
		}
	}
}

fn test_multi_value_array_guard_preserves_sibling_smartcasts() {
	values := ['payload']
	for index in [0, 1] {
		mut current := GuardSmartcastValue(GuardSmartcastText{'narrowed'})
		replacement := GuardSmartcastValue(GuardSmartcastNumber{17})
		if mut current is GuardSmartcastText {
			first, second := if value := values[index] {
				current = replacement
				value, 'selected'
			} else {
				guard_smartcast_text(current), 'missing'
			}
			if index == 0 {
				assert first == 'payload' && second == 'selected'
				assert guard_smartcast_kind(current) == 'number'
			} else {
				assert first == 'narrowed' && second == 'missing'
				assert guard_smartcast_kind(current) == 'text'
			}
		}
	}
}

fn test_multi_value_option_guard_preserves_sibling_smartcasts() {
	for present in [false, true] {
		mut current := GuardSmartcastValue(GuardSmartcastText{'narrowed'})
		replacement := GuardSmartcastValue(GuardSmartcastNumber{17})
		if mut current is GuardSmartcastText {
			first, second := if value := guard_smartcast_option(present) {
				current = replacement
				value, 'selected'
			} else {
				current.read_text(), 'missing'
			}
			if present {
				assert first == 'payload' && second == 'selected'
				assert guard_smartcast_kind(current) == 'number'
			} else {
				assert first == 'narrowed' && second == 'missing'
				assert guard_smartcast_kind(current) == 'text'
			}
		}
	}
}

fn test_multi_value_result_guard_preserves_sibling_smartcasts() {
	for present in [false, true] {
		mut current := GuardSmartcastValue(GuardSmartcastText{'narrowed'})
		replacement := GuardSmartcastValue(GuardSmartcastNumber{17})
		if mut current is GuardSmartcastText {
			first, second := if value := guard_smartcast_result(present) {
				current = replacement
				value, 'selected'
			} else {
				guard_smartcast_text(current), err.msg()
			}
			if present {
				assert first == 'payload' && second == 'selected'
				assert guard_smartcast_kind(current) == 'number'
			} else {
				assert first == 'narrowed' && second == 'missing'
				assert guard_smartcast_kind(current) == 'text'
			}
		}
	}
}

fn test_multi_value_channel_guard_preserves_sibling_smartcasts() {
	for present in [false, true] {
		mut values := chan string{cap: 1}
		if present {
			values <- 'payload'
		}
		values.close()
		mut current := GuardSmartcastValue(GuardSmartcastText{'narrowed'})
		replacement := GuardSmartcastValue(GuardSmartcastNumber{17})
		if mut current is GuardSmartcastText {
			first, second := if value := <-values {
				current = replacement
				value, 'selected'
			} else {
				guard_smartcast_text(current), 'missing'
			}
			if present {
				assert first == 'payload' && second == 'selected'
				assert guard_smartcast_kind(current) == 'number'
			} else {
				assert first == 'narrowed' && second == 'missing'
				assert guard_smartcast_kind(current) == 'text'
			}
		}
	}
}

fn test_multi_value_guard_else_if_preserves_sibling_smartcasts() {
	values := {
		'present': 'payload'
	}
	for key in ['present', 'missing'] {
		mut current := GuardSmartcastValue(GuardSmartcastText{'narrowed'})
		replacement := GuardSmartcastValue(GuardSmartcastNumber{17})
		if mut current is GuardSmartcastText {
			mut first, mut second := '', ''
			first, second = if value := values[key] {
				current = replacement
				value, 'selected'
			} else if value := guard_smartcast_option(false) {
				value, 'unused'
			} else {
				guard_smartcast_text(current), 'fallback'
			}
			if key == 'present' {
				assert first == 'payload' && second == 'selected'
				assert guard_smartcast_kind(current) == 'number'
			} else {
				assert first == 'narrowed' && second == 'fallback'
				assert guard_smartcast_kind(current) == 'text'
			}
		}
	}
}

fn test_multi_value_guard_merges_else_invalidation() {
	values := {
		'present': 'payload'
	}
	for key in ['present', 'missing'] {
		mut current := GuardSmartcastValue(GuardSmartcastText{'narrowed'})
		replacement := GuardSmartcastValue(GuardSmartcastNumber{17})
		if mut current is GuardSmartcastText {
			first, second := if value := values[key] {
				guard_smartcast_text(current), value
			} else {
				current = replacement
				'changed', 'missing'
			}
			if key == 'present' {
				assert first == 'narrowed' && second == 'payload'
				assert guard_smartcast_kind(current) == 'text'
			} else {
				assert first == 'changed' && second == 'missing'
				assert guard_smartcast_kind(current) == 'number'
			}
		}
	}
}
