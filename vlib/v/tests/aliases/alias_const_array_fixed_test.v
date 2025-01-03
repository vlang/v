module main

const image_width = 800
const image_height = 400

struct Colour {
	r u8
	g u8
	b u8
}

const buffer_size = image_width * image_height * sizeof[Colour]()

type ImageBuffer = [buffer_size]Colour

fn test_main() {
	t := ImageBuffer{}
	assert t.len == buffer_size
}
