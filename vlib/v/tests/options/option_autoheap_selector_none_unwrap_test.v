struct Padding {
	top    f32
	bottom f32
}

fn (p Padding) height() f32 {
	return p.top + p.bottom
}

struct Shape {
	height  f32
	padding Padding
}

@[heap]
struct Layout {
	shape &Shape
}

fn find_layout(layout &Layout) ?Layout {
	return *layout
}

fn test_autoheap_option_selector_none_unwrap() {
	shape := &Shape{
		height:  7
		padding: Padding{
			top:    1
			bottom: 2
		}
	}
	layout := &Layout{
		shape: shape
	}
	layout_scroll := find_layout(layout)
	if layout_scroll != none {
		layout_scroll_height := layout_scroll.shape.height - layout_scroll.shape.padding.height()
		assert layout_scroll_height == 4
	} else {
		assert false
	}
}
