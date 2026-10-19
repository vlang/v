module ifoptdom

import ifopturl

// StyleValue is a struct whose short name matches the `ifoptstyle.StyleValue` sum type.
@[heap]
pub struct StyleValue {
pub:
	n int
}

// Origin is a struct whose short name matches the `ifopturl.Origin` sum type.
pub struct Origin {
pub:
	scheme string
}

pub struct Document {
pub:
	origin_ Origin
	url     ifopturl.Origin = ifopturl.TupleOrigin{}
}

// origin returns the document origin.
pub fn (d &Document) origin() Origin {
	return d.origin_
}

// Frame is a heap struct referenced through `?&Frame`.
@[heap]
pub struct Frame {
pub:
	width int
}

// Frame.new returns a frame of the given width.
pub fn Frame.new(width int) &Frame {
	return &Frame{width}
}
