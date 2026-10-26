module main

// Buttons are flags for the mouse buttons.
@[flag]
pub enum Buttons {
	left
	right
}

// Options are the options for `draw`.
@[params]
pub struct Options {
pub:
	width int
}

// draw is shown with the attributes above its signature.
@[deprecated: 'use pub fn paint instead of <b>draw</b> & co']
@[deprecated_after: '2999-01-01']
pub fn draw(opts Options) {}

// press is a method with an attribute.
@[inline]
pub fn (b Buttons) press() {}
