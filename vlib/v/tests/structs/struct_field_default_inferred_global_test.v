// vtest vflags: -enable-globals
__global active_theme = default_theme

const default_theme = make_theme()

struct Shade {
	value int
}

struct Style {
	shade &Shade
}

struct Theme {
	style Style
}

fn make_theme() Theme { return Theme{ style: Style{ shade: &Shade{42} } } }

struct Widget {
	shade &Shade = active_theme.style.shade
}

fn test_global_inferred_from_a_const_is_available_to_field_defaults() {
	assert Widget{}.shade.value == 42
	assert active_theme.style.shade.value == 42
}
