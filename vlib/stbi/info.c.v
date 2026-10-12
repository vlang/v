module stbi

fn C.stbi_info_from_memory(buffer &u8, len i32, x &int, y &int, channels_in_file &int) i32

// ImageInfo describes an image, as read from its header alone.
pub struct ImageInfo {
pub:
	width       int // the width in pixels
	height      int // the height in pixels
	nr_channels int // the number of color channels in the file
}

// info_from_memory reads the header of the image in `buf`, without decoding its
// pixels, and returns the dimensions and the number of channels it declares.
// `bufsize` reaches C as an `int`, so a buffer larger than `max_i32` is refused
// rather than read as its lower 32 bits.
pub fn info_from_memory(buf &u8, bufsize int) !ImageInfo {
	if bufsize < 0 || bufsize > max_i32 {
		return error('stbi_image cannot read a header from a buffer of ${bufsize} bytes')
	}
	mut width := 0
	mut height := 0
	mut nr_channels := 0
	if 0 == C.stbi_info_from_memory(buf, i32(bufsize), &width, &height, &nr_channels) {
		return error('stbi_image failed to read the image header from memory')
	}
	return ImageInfo{
		width:       width
		height:      height
		nr_channels: nr_channels
	}
}
