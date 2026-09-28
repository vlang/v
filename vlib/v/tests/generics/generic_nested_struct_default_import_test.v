module main

import v.tests.generics.generic_default_modules.nesteddefaults

struct Local {
	x int = 11
}

enum DefaultMode {
	first = 20
	second
}

type DefaultChoice = int | string

struct LocalPointerBox[T] {
	value &T = &T{}
}

fn test_imported_nested_generic_struct_defaults_use_local_type() {
	box := nesteddefaults.Box[Local]{}
	assert box.value.x == 11
	wrapper := nesteddefaults.Wrapper[Local]{}
	assert wrapper.box.value.x == 11
	pointer := nesteddefaults.PointerBox[Local]{}
	assert pointer.value.x == 11
	local_pointer := LocalPointerBox[Local]{}
	assert local_pointer.value.x == 11
}

fn test_imported_generic_heap_array_default_initializes_metadata() {
	pointer := nesteddefaults.PointerBox[[]string]{}
	assert pointer.value.element_size == sizeof(string)
}

fn test_imported_generic_enum_default_uses_first_member() {
	box := nesteddefaults.Box[DefaultMode]{}
	assert box.value == .first
}

fn test_imported_generic_sum_default_uses_first_variant() {
	box := nesteddefaults.Box[DefaultChoice]{}
	assert box.value is int
	assert box.value as int == 0
}

fn test_imported_generic_fixed_array_default_preserves_elements() {
	box := nesteddefaults.Box[[1]nesteddefaults.Foo]{}
	assert box.value[0].a == 10
}

fn test_imported_generic_heap_sum_default_uses_first_variant() {
	pointer := nesteddefaults.PointerBox[DefaultChoice]{}
	assert pointer.value is int
	assert pointer.value as int == 0
}

fn test_imported_generic_heap_fixed_array_default_preserves_elements() {
	pointer := nesteddefaults.PointerBox[[1]nesteddefaults.Foo]{}
	assert pointer.value[0].a == 10
}
