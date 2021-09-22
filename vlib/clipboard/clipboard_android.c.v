module clipboard

import clipboard.dummy

pub type Clipboard = dummy.Clipboard

fn new_clipboard() &Clipboard {
	return dummy.new_clipboard()
}

// new_primary returns a new X11 `PRIMARY` type `Clipboard` instance allocated on the heap.
// Please note: new_primary only works on X11 based systems.
pub fn new_primary() &Clipboard {
	return dummy.new_primary()
}
