module imported_defaults_mod

import imported_defaults_dep

pub struct LengthOrAuto {
pub mut:
	length ?f64 = none
}

pub struct LengthBox {
pub mut:
	top    LengthOrAuto
	bottom LengthOrAuto
}

pub struct Command {
pub mut:
	kind      string
	x         f64
	backdrop  ?imported_defaults_dep.Color = none
	index     ?int                         = none
	font_size f64 = 16.0
	inset     LengthBox
	transform [6]f64
}

pub struct Cache {
pub mut:
	inset LengthBox
	width int = 3
}

// new_cache allocates a cache with its nested `none` defaults in this module.
pub fn new_cache() &Cache {
	return &Cache{}
}

pub struct Panel {
pub mut:
	id         int
	horizontal imported_defaults_dep.Scrollbar = imported_defaults_dep.Scrollbar.create(.horizontal)
	vertical   imported_defaults_dep.Scrollbar = imported_defaults_dep.Scrollbar.create(.vertical)
}
