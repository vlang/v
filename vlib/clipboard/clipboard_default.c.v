module clipboard

import clipboard.x11

// Clipboard represents a system clipboard.
//
// System "copy" and "paste" actions utilize the clipboard for temporary storage.
pub type Clipboard = x11.Clipboard

fn new_clipboard() &Clipboard {
	return x11.new_clipboard()
}

// new_primary returns a new X11 `PRIMARY` type `Clipboard` instance allocated on the heap.
// Please note: new_primary only works on X11 based systems.
pub fn new_primary() &Clipboard {
	return x11.new_primary()
}
