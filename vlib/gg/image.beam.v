// BEAM Backend: Graphics/GPU modules are not applicable on the BEAM VM.
// The BEAM backend focuses on server-side, networking, and concurrent workloads.
// All functions return safe defaults (empty structs, 0, false).
// For GUI on BEAM, consider using wx (Erlang's native GUI toolkit).
//
// Image loading requires stbi (C library) which is not available on BEAM.
// create_image* functions return errors; draw_image* functions are no-ops.
module gg

import sokol.gfx

pub struct ImageDesc {
pub:
	id int
}

@[heap; markused]
pub struct Image {
pub mut:
	id          int
	width       int
	height      int
	nr_channels int
	ok          bool
	data        voidptr
	ext         string
	simg_ok     bool
	simg        gfx.Image
	ssmp        gfx.Sampler
	path        string
}

fn (image &Image) destroy() {
}

pub fn (mut img Image) init_sokol_image() &Image {
	return img
}

pub fn (mut ctx Context) create_image(file string) !Image {
	return error('gg.create_image not available on BEAM')
}

pub fn (mut ctx Context) create_image_from_memory(buf &u8, bufsize int) !Image {
	return error('gg.create_image_from_memory not available on BEAM')
}

pub fn (mut ctx Context) create_image_from_byte_array(b []u8) !Image {
	return error('gg.create_image_from_byte_array not available on BEAM')
}

@[deprecated]
pub fn (mut ctx Context) create_image_with_size(file string, width int, height int) Image {
	return Image{}
}

fn create_image(file string) Image {
	return Image{}
}

pub enum PixelFormat {
	rgba8
}

pub enum Wrap {
	clamp_to_edge
}

pub enum Filter {
	linear
}

pub struct StreamingImageConfig {
pub:
	pixel_format PixelFormat = .rgba8
	wrap_u       Wrap        = .clamp_to_edge
	wrap_v       Wrap        = .clamp_to_edge
	min_filter   Filter      = .linear
	mag_filter   Filter      = .linear
	num_mipmaps  int         = 1
	num_slices   int         = 1
}

pub fn (mut ctx Context) new_streaming_image(w int, h int, channels int, sicfg StreamingImageConfig) int {
	return 0
}

pub fn (mut ctx Context) update_pixel_data(cached_image_idx int, buf &u8) {
}

pub fn (mut img Image) update_pixel_data(buf &u8) {
}

pub fn (ctx &Context) draw_image(x f32, y f32, width f32, height f32, img_ &Image) {
}

pub fn (ctx &Context) draw_image_with_config(config DrawImageConfig) {
}
