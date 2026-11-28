// The message of a failed `assert` prints an empty interface, which covers every
// type that it can hold, `?Named` among them. That case must read the value as a
// `?Named`, not as the placeholder option type.
interface Holder {}

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

fn test_empty_interface_str_compiles_next_to_an_optional_interface() {
	held := Holder(1)
	arr := [&held, &held]
	assert arr[0] == arr[1]
	record := &Record{
		label: 'record'
	}
	assert optional_alias_name(record) == 'record'
	wrapped := ?Named(none)
	assert optional_alias_name(wrapped) == 'none'
}
