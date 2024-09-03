module main

import structs_with_noinit

enum RenderableKind {
	circle
	image
	rect
}

union RenderableValue {
	circle structs_with_noinit.Circle
	image  structs_with_noinit.Image
	rect   structs_with_noinit.Rect
}

struct Renderable {
	RenderableValue
	kind RenderableKind
}

fn draw(r Renderable) int {
	match r.kind {
		.circle {
			println('()')
			return 999
		}
		.rect {
			println('[]')
		}
		.image {
			println('o_O')
		}
	}
	return 1
}

fn test_initialisation_of_a_struct_containing_embedded_union_field() {
	r := Renderable{
		kind:   .circle
		circle: structs_with_noinit.make_circle()
	}
	assert draw(r) == 999
}
