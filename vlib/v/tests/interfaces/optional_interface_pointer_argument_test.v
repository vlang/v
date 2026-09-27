interface Named {
	name() string
}

struct Record {
	label string
}

fn (r &Record) name() string { return r.label }

type MaybeNamed = ?Named

fn optional_name(item ?Named) string {
	value := item or { return 'none' }
	return value.name()
}

fn optional_alias_name(item MaybeNamed) string {
	value := item or { return 'none' }
	return value.name()
}

fn test_pointer_argument_is_boxed_inside_optional_interface() {
	item := &Record{ label: 'record' }
	assert optional_name(item) == 'record'
	assert optional_name(&Record{ label: 'inline' }) == 'inline'
	assert optional_alias_name(item) == 'record'
	assert optional_name(none) == 'none'
	wrapped := ?Named(item)
	assert optional_name(wrapped) == 'record'
}
