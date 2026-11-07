enum FixedDefaultState {
	ready = 7
	done
}

struct FixedDefaultItem {
	value int = 42
	state FixedDefaultState
}

type FixedDefaultItems = [2]FixedDefaultItem | int
type NestedFixedDefaultItems = [2][2]FixedDefaultItem | int
type FixedDefaultPair = [2]FixedDefaultItem
type AliasedFixedDefaultItems = FixedDefaultPair | int
type FixedDefaultStates = [2]FixedDefaultState | int

struct FixedDefaultHolder {
	items  FixedDefaultItems
	nested NestedFixedDefaultItems
	alias  AliasedFixedDefaultItems
	states FixedDefaultStates
}

fn fixed_default_value[T]() T {
	return T{}
}

fn fixed_default_pointer[T]() &T {
	return &T{}
}

fn test_fixed_array_sumtype_defaults_preserve_element_values() {
	holder := FixedDefaultHolder{}
	assert holder.items is [2]FixedDefaultItem
	items := holder.items as [2]FixedDefaultItem
	for item in items {
		assert item.value == 42
		assert item.state == .ready
	}
	nested := holder.nested as [2][2]FixedDefaultItem
	for row in nested {
		for item in row {
			assert item.value == 42
			assert item.state == .ready
		}
	}
	alias := holder.alias as FixedDefaultPair
	for item in alias {
		assert item.value == 42
		assert item.state == .ready
	}
	assert (holder.states as [2]FixedDefaultState) == [FixedDefaultState.ready, .ready]!
}

fn test_generic_fixed_array_sumtype_defaults_preserve_element_values() {
	value := fixed_default_value[FixedDefaultItems]()
	assert value is [2]FixedDefaultItem
	assert (value as [2]FixedDefaultItem)[1].value == 42
	pointer := fixed_default_pointer[FixedDefaultItems]()
	pointed := *pointer
	assert pointed is [2]FixedDefaultItem
	assert (pointed as [2]FixedDefaultItem)[0].state == .ready
}
