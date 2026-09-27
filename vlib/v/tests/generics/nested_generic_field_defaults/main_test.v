import defaults

struct NestedImportedDefaults {
	integer  defaults.Number[int]
	floating defaults.Number[f64]
	state    defaults.State
}

fn test_imported_nested_generic_defaults() {
	value := NestedImportedDefaults{}
	assert value.integer.value == 42
	assert value.floating.value == 42.0
	assert value.state.undo.max_size == 50
	assert value.state.redo.max_size == 50
	assert defaults.State{}.undo.max_size == 50
	assert defaults.Number[int]{ value: 0 }.value == 0
}
