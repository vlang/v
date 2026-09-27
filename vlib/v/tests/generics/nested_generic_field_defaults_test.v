struct Stack[T] {
mut:
	elements []T
	max_size int = 50
}

struct State {
mut:
	undo Stack[string]
	redo Stack[string]
}

struct Wrapper {
	state State
}

struct NumericBox[T] {
	value T = T(42)
}

struct Numbers {
	integer  NumericBox[int]
	floating NumericBox[f64]
}

fn state_or_default(states map[int]State, key int) State {
	return states[key] or { State{} }
}

fn test_nested_generic_field_defaults() {
	state := State{}
	assert state.undo.max_size == 50
	assert state.redo.max_size == 50
	assert state.undo.elements.len == 0
	nested := Wrapper{}
	assert nested.state.undo.max_size == 50
	empty := map[int]State{}
	from_map := state_or_default(empty, 1)
	assert from_map.undo.max_size == 50
	overridden := State{ undo: Stack[string]{ max_size: 0 } }
	assert overridden.undo.max_size == 0
	assert overridden.redo.max_size == 50
	numbers := Numbers{}
	assert numbers.integer.value == 42
	assert numbers.floating.value == 42.0
}
