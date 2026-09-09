module main

import imported_interface_auto_eq_18326.vest

struct WebModule {
	controllers []vest.Object
}

fn test_imported_interface_array_field_auto_eq() {
	mut mod := WebModule{}
	assert mod == mod
}

fn test_imported_interface_equality_helper_deduping() {
	assert exercise_imported_interface_equality_helpers()
}
