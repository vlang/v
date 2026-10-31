import imported_defaults_dep
import imported_defaults_mod

// Field defaults of imported structs, and of structs nested in another struct's
// zero value, are emitted by cgen rather than lowered by the transformer. A
// `none` default there has no checker-recorded type, and was emitted as the
// untyped `(__v_option){.ok = false}`, which C rejects for a `?T` field.

struct Length {
mut:
	value ?f64 = none
}

struct Edges {
mut:
	top    Length
	bottom Length
}

struct Layout {
mut:
	edges Edges
	gap   int = 4
}

fn copy_command(source imported_defaults_mod.Command) imported_defaults_mod.Command {
	return imported_defaults_mod.Command{
		x:         1.5
		transform: source.transform
	}
}

fn test_imported_struct_none_defaults() {
	command := imported_defaults_mod.Command{}
	assert command.backdrop == none
	assert command.index == none
	assert command.font_size == 16.0
	assert command.inset.top.length == none
	assert command.inset.bottom.length == none
}

fn test_imported_struct_none_defaults_with_fixed_array_field() {
	mut source := imported_defaults_mod.Command{
		backdrop: imported_defaults_dep.Color{
			r: 9
		}
		index:    2
	}
	source.transform[2] = 3.0
	copy := copy_command(source)
	assert copy.x == 1.5
	assert copy.transform[2] == 3.0
	assert copy.backdrop == none
	assert copy.index == none
	assert copy.font_size == 16.0
}

fn test_nested_struct_none_defaults_in_declaring_module() {
	cache := imported_defaults_mod.new_cache()
	assert cache.width == 3
	assert cache.inset.top.length == none
	assert cache.inset.bottom.length == none
}

fn test_nested_struct_none_defaults_in_main() {
	layout := &Layout{}
	assert layout.gap == 4
	assert layout.edges.top.value == none
	assert layout.edges.bottom.value == none
	mut value := Layout{}
	value.edges.top.value = 2.5
	assert value.edges.top.value or { 0.0 } == 2.5
	assert value.edges.bottom.value == none
}
