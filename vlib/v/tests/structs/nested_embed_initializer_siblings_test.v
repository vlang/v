struct Dimensions {
	width  int = 7
	height int = 11
}

struct Accessibility {
	label string = 'default'
}

struct Config {
	Dimensions
	Accessibility
	axis int = 1
}

struct View {
	Config
	visible bool = true
}

struct NestedView {
	View
}

struct PointerView {
	Config
}

fn test_nested_embed_initializers_keep_sibling_paths() {
	view := View{ axis: 2, width: 42, label: 'content' }
	assert view.axis == 2
	assert view.width == 42
	assert view.height == 11
	assert view.label == 'content'
	assert view.visible

	nested := NestedView{ width: 63, label: 'nested', axis: 3, visible: false }
	assert nested.width == 63
	assert nested.height == 11
	assert nested.label == 'nested'
	assert nested.axis == 3
	assert !nested.visible

	pointer := PointerView{ label: 'pointer', width: 84, axis: 4 }
	assert pointer.width == 84
	assert pointer.height == 11
	assert pointer.axis == 4
	assert pointer.label == 'pointer'
}
