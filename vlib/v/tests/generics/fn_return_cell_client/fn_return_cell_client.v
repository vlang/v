module fn_return_cell_client

import fn_return_cell_mod

// empty reports whether an imported callback cell contains nil.
pub fn empty[T]() bool {
	cell := fn_return_cell_mod.Cell[fn () T]{}
	return cell.value == unsafe { nil }
}
