module gg

@[heap]
pub struct Image {
pub mut:
	id          int
	width       int
	height      int
	nr_channels int
	ok          bool
	data        voidptr
	ext         string

	path string
}

pub fn (ctx &Context) draw_image_with_config(config DrawImageConfig) {
}

// destroy GPU resources associated with the image
fn (img &Image) destroy() {
}
