module fields

pub struct RefEntry {
pub mut:
	value string
}

@[minify]
struct Field {
mut:
	value         string
	initial_value string
	dirty         bool
}

@[minify]
struct State {
mut:
	fields map[string]Field
}

fn new_state() State {
	return State{
		fields: {
			'email': Field{ value: 'bad', initial_value: 'seed', dirty: true }
		}
	}
}

fn reset() State {
	mut state := new_state()
	mut values := map[string]string{}
	for id, mut field in state.fields {
		field.value = field.initial_value
		field.dirty = false
		state.fields[id] = field
		values[id] = field.initial_value
	}
	assert values['email'] == 'seed'
	return state
}

// validate_reset checks copying a mutable map value from another module.
pub fn validate_reset() {
	state := reset()
	field := state.fields['email'] or { panic('missing') }
	assert field.value == 'seed'
	assert field.initial_value == 'seed'
	assert !field.dirty
}

// validate_pointer_values checks that explicit pointer map values keep their identity.
pub fn validate_pointer_values() {
	first := &Field{ value: 'first' }
	mut entries := {
		'first': first
	}
	for key, mut value in entries {
		entries[key] = value
	}
	assert entries['first'] == first
}

// validate_reference_iteration checks that copies of explicit map references still alias entries.
pub fn validate_reference_iteration() {
	mut entries := {
		'first': Field{ value: 'before' }
	}
	mut references := []&Field{}
	for _, value in &entries {
		alias := value
		references << alias
	}
	view := &entries
	for _, value in view {
		alias := value
		references << alias
	}
	entries['first'].value = 'after'
	assert references.len == 2
	assert references[0] == references[1]
	for reference in references {
		assert reference.value == 'after'
	}
}
