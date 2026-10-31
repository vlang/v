module main

// Sides are the sides of a shape that have a border.
@[flag]
pub enum Sides {
	top
	right
	bottom
	left
}

// Fill is how a shape is filled.
pub enum Fill {
	none
	solid
}

// Hidden is not public, so it and its helpers are only shown with `-all`.
enum Hidden {
	a
	b
}

// Shade has its own `from`, which replaces the generated one.
pub enum Shade {
	light
	dark
}

// Shade.from returns the shade called `name`.
pub fn Shade.from(name string) Shade {
	return if name == 'dark' { .dark } else { .light }
}
