// Test for issue #25478
// Using the same generic interface with different type parameters in a function
// should correctly resolve the return types of interface methods.

interface Bug[T] {
	buggy() T
}

struct StringBug implements Bug[string] {
	value string
}

fn (s StringBug) buggy() string {
	return s.value
}

struct StringArrayBug implements Bug[[]string] {
	values []string
}

fn (s StringArrayBug) buggy() []string {
	return s.values
}

fn list(items Bug[[]string], selected Bug[string], generator fn (arg string, selected bool) string) []string {
	value := items.buggy() // should be []string
	children := value.map(generator(it, it == selected.buggy()))
	return children
}

fn test_generic_interface_with_different_type_params() {
	items := StringArrayBug{
		values: ['Hello', 'Hola']
	}
	selected := StringBug{
		value: 'Hola'
	}
	result := list(items, selected, fn (arg string, is_selected bool) string {
		if is_selected {
			return '--${arg}'
		}
		return arg
	})
	assert result.len == 2
	assert result[0] == 'Hello'
	assert result[1] == '--Hola'
}

fn test_generic_interface_method_return_types() {
	items := StringArrayBug{
		values: ['a', 'b', 'c']
	}
	selected := StringBug{
		value: 'b'
	}

	// Verify return types are correctly resolved
	arr := items.buggy()
	assert arr.len == 3
	assert arr[0] == 'a'

	str := selected.buggy()
	assert str == 'b'
}
