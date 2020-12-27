module clipboard

import clipboard.dummy

pub type Clipboard = dummy.Clipboard

fn new_clipboard() &Clipboard {
	return dummy.new_clipboard()
}

pub fn new_primary() &Clipboard {
	return dummy.new_primary()
}
