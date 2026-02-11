// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
//
// stbi is a C image loading library. On BEAM, image load/write functions return errors.
// Configuration functions (flip, compression level) are no-ops.
module stbi

pub struct Image {
pub mut:
	width                int
	height               int
	nr_channels          int
	ok                   bool
	data                 &u8 = unsafe { nil }
	ext                  string
	original_nr_channels int
}

@[params]
pub struct LoadParams {
pub:
	desired_channels int = 4
}

pub fn set_flip_vertically_on_load(val bool) {}

pub fn set_flip_vertically_on_write(val bool) {}

pub fn set_png_compression_level(level int) {}

pub fn write_force_png_filter(level int) {}

pub fn write_tga_with_rle(flag bool) {}

pub fn (img &Image) free() {}

pub fn load(path string, params LoadParams) !Image {
	return error('stbi not available on BEAM')
}

pub fn load_from_memory(buf &u8, bufsize int, params LoadParams) !Image {
	return error('stbi not available on BEAM')
}

pub fn resize_uint8(img &Image, output_w int, output_h int) !Image {
	return error('stbi not available on BEAM')
}

pub fn stbi_write_png(path string, w int, h int, comp int, buf &u8, row_stride_in_bytes int) ! {
	return error('stbi not available on BEAM')
}

pub fn stbi_write_bmp(path string, w int, h int, comp int, buf &u8) ! {
	return error('stbi not available on BEAM')
}

pub fn stbi_write_tga(path string, w int, h int, comp int, buf &u8) ! {
	return error('stbi not available on BEAM')
}

pub fn stbi_write_jpg(path string, w int, h int, comp int, buf &u8, quality int) ! {
	return error('stbi not available on BEAM')
}
