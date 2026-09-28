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

struct BoxArray {
	boxes [2]NumericBox[int]
	grid  [2][2]NumericBox[int]
}

type NumericIntAlias = NumericBox[int]

struct AliasedNumbers {
	integer NumericIntAlias
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
	arrays := BoxArray{}
	assert arrays.boxes[0].value == 42
	assert arrays.boxes[1].value == 42
	assert arrays.grid[0][1].value == 42
	assert arrays.grid[1][0].value == 42
	aliased := AliasedNumbers{}
	assert aliased.integer.value == 42
}

fn test_runtime_array_initializes_fixed_array_generic_fields() {
	count := 2
	values := []BoxArray{len: count}
	assert values.len == count
	for value in values {
		assert value.boxes[0].value == 42
		assert value.boxes[1].value == 42
		assert value.grid[0][1].value == 42
		assert value.grid[1][0].value == 42
	}
	direct := [][2]NumericBox[int]{len: count}
	assert direct[0][0].value == 42
	assert direct[1][1].value == 42
}

type NumericBoxes = [2]NumericBox[int]
type NumericGrid = [2]NumericBoxes

struct AliasedBoxArrays {
	boxes NumericBoxes
	grid  NumericGrid
}

fn test_aliased_fixed_array_fields_keep_generic_defaults() {
	value := AliasedBoxArrays{}
	assert value.boxes[0].value == 42
	assert value.boxes[1].value == 42
	assert value.grid[0][1].value == 42
	assert value.grid[1][0].value == 42
	values := []AliasedBoxArrays{len: 2}
	for item in values {
		assert item.boxes[1].value == 42
		assert item.grid[1][1].value == 42
	}
}
