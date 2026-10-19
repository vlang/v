// vtest vflags: -enable-globals
@[translated]
module main

// Translated code permits local guards to shadow global assignment destinations.
__global (
	value          string
	first          string
	second         string
	selector_value BindingScopeDestination
	indexed_value  []string
)

fn binding_scope_option(present bool) ?string {
	if present {
		return 'payload'
	}
	return none
}

fn binding_scope_result(present bool) !string {
	if present {
		return 'payload'
	}
	return error('missing')
}

fn binding_scope_pair(left string, right string) (string, string) {
	return left, right
}

fn binding_scope_option_pair(present bool) ?(string, string) {
	if present {
		return 'first', 'second'
	}
	return none
}

fn test_option_guard_assigns_outer_binding_name() {
	for present in [false, true] {
		value = 'outer'
		mut tag := ''
		value, tag = if value := binding_scope_option(present) {
			value + '-selected', value
		} else {
			'fallback', 'none'
		}
		assert value == if present { 'payload-selected' } else { 'fallback' }
		assert tag == if present { 'payload' } else { 'none' }
	}
}

fn test_result_guard_assigns_outer_binding_name() {
	for present in [false, true] {
		value = 'outer'
		mut tag := ''
		value, tag = if value := binding_scope_result(present) {
			value + '-selected', value
		} else {
			'fallback', err.msg()
		}
		assert value == if present { 'payload-selected' } else { 'fallback' }
		assert tag == if present { 'payload' } else { 'missing' }
	}
}

fn test_map_guard_assigns_outer_binding_name() {
	values := {
		'present': 'payload'
	}
	for key in ['missing', 'present'] {
		value = 'outer'
		mut tag := ''
		value, tag = if value := values[key] {
			value, 'selected'
		} else {
			'fallback', 'missing'
		}
		assert value == if key == 'present' { 'payload' } else { 'fallback' }
		assert tag == if key == 'present' { 'selected' } else { 'missing' }
	}
}

fn test_array_guard_assigns_outer_binding_name() {
	values := ['payload']
	for index in [1, 0] {
		value = 'outer'
		mut tag := ''
		value, tag = if value := values[index] {
			value, 'selected'
		} else {
			'fallback', 'missing'
		}
		assert value == if index == 0 { 'payload' } else { 'fallback' }
		assert tag == if index == 0 { 'selected' } else { 'missing' }
	}
}

fn test_channel_guard_assigns_outer_binding_name() {
	for present in [false, true] {
		mut values := chan string{cap: 1}
		if present {
			values <- 'payload'
		}
		values.close()
		value = 'outer'
		mut tag := ''
		value, tag = if value := <-values {
			value, 'selected'
		} else {
			'fallback', 'missing'
		}
		assert value == if present { 'payload' } else { 'fallback' }
		assert tag == if present { 'selected' } else { 'missing' }
	}
}

fn test_option_tuple_guard_assigns_outer_binding_names() {
	for present in [false, true] {
		first = 'outer-first'
		second = 'outer-second'
		first, second = if first, second := binding_scope_option_pair(present) {
			second, first
		} else {
			'fallback', 'missing'
		}
		assert first == if present { 'second' } else { 'fallback' }
		assert second == if present { 'first' } else { 'missing' }
	}
}

fn test_guard_call_tail_assigns_outer_binding_name() {
	for present in [false, true] {
		value = 'outer'
		mut tag := ''
		value, tag = if value := binding_scope_result(present) {
			binding_scope_pair(value, value + '-tag')
		} else {
			binding_scope_pair('fallback', err.msg())
		}
		assert value == if present { 'payload' } else { 'fallback' }
		assert tag == if present { 'payload-tag' } else { 'missing' }
	}
}

fn test_guard_nested_tail_assigns_outer_binding_name() {
	for chosen in [false, true] {
		value = 'outer'
		mut tag := ''
		value, tag = if value := binding_scope_option(true) {
			if chosen { value, 'chosen' } else { value + '-other', 'other' }
		} else {
			'fallback', 'missing'
		}
		assert value == if chosen { 'payload' } else { 'payload-other' }
		assert tag == if chosen { 'chosen' } else { 'other' }
	}
}

struct BindingScopeDestination {
mut:
	text string
}

fn test_guard_selector_target_uses_outer_binding_type() {
	for present in [false, true] {
		selector_value = BindingScopeDestination{'outer'}
		mut tag := ''
		selector_value.text, tag = if selector_value := binding_scope_option(present) {
			selector_value, 'selected'
		} else {
			'fallback', 'none'
		}
		assert selector_value.text == if present { 'payload' } else { 'fallback' }
		assert tag == if present { 'selected' } else { 'none' }
	}
}

fn test_guard_index_target_uses_outer_binding_type() {
	for present in [false, true] {
		indexed_value = ['outer']
		mut tag := ''
		indexed_value[0], tag = if indexed_value := binding_scope_result(present) {
			indexed_value, 'selected'
		} else {
			'fallback', err.msg()
		}
		assert indexed_value == [if present { 'payload' } else { 'fallback' }]
		assert tag == if present { 'selected' } else { 'missing' }
	}
}
