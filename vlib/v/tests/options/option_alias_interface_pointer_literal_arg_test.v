interface Named {
	name() string
}

struct Record {
	label string
}

fn (r &Record) name() string {
	return r.label
}

type MaybeNamed = ?Named

fn optional_alias_name(item MaybeNamed) string {
	value := item or { return 'none' }
	return value.name()
}

// A pointer literal passed for an alias of an option of an interface is boxed in
// the interface, as one passed for `?Named` is.
fn test_pointer_literal_is_boxed_for_an_option_alias_of_an_interface() {
	item := &Record{
		label: 'var'
	}
	assert optional_alias_name(item) == 'var'
	assert optional_alias_name(&Record{ label: 'literal' }) == 'literal'
}
