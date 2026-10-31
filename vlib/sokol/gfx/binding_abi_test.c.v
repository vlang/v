module gfx

fn read_pixel_width(value &i32) i32 {
	return *value
}

fn accept_image_initializer(callback fn (Image, &ImageDesc)) {
	_ = callback
}

fn test_pixel_format_metadata_has_native_field_types() {
	info := PixelFormatInfo{
		compressed:      true
		bytes_per_pixel: 16
	}
	assert info.compressed
	assert typeof(info.bytes_per_pixel).name == 'i32'
	assert sizeof(typeof(info.bytes_per_pixel)) == 4
	assert read_pixel_width(&info.bytes_per_pixel) == 16
}

fn test_image_initializer_accepts_image_descriptor() {
	accept_image_initializer(C.sg_init_image)
}
