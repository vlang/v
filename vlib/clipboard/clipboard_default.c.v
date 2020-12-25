module clipboard

import clipboard.x11

pub type Clipboard = x11.Clipboard

fn new_clipboard() &Clipboard {
	return x11.new_clipboard()
}

pub fn new_primary() &Clipboard {
	return x11.new_primary()
}
