import imported_defaults_dep
import imported_defaults_mod

// An imported struct's field default that calls a static method of a type from
// a third module (`dep.Scrollbar.create(.horizontal)`) is emitted by cgen from
// its raw selector form. It was called as `dep__Scrollbar__create`, a function
// that is never declared: static methods are emitted under their static fn name.

fn test_imported_struct_default_calls_static_method_of_imported_type() {
	panel := imported_defaults_mod.Panel{
		id: 2
	}
	assert panel.id == 2
	assert panel.horizontal.direction == .horizontal
	assert panel.vertical.direction == .vertical
	assert panel.vertical.width == 7
}

fn test_explicit_field_overrides_static_method_default() {
	panel := imported_defaults_mod.Panel{
		vertical: imported_defaults_dep.Scrollbar{
			direction: .horizontal
			width:     3
		}
	}
	assert panel.horizontal.width == 7
	assert panel.vertical.direction == .horizontal
	assert panel.vertical.width == 3
}
