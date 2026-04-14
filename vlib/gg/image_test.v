// vtest build: !docker-ubuntu-musl // needs GL/gl.h
import os
import gg

const vroot = @VEXEROOT
const background_path = os.join_path(vroot, 'examples/flappylearning/assets/img/background.png')

fn test_get_cached_image_idx_bounds_checking() {
	mut ctx := gg.new_context(width: 100)
	assert ctx.get_cached_image_by_idx(-1).ok == false
	assert ctx.get_cached_image_by_idx(1).ok == false
}

fn test_remove_cached_image_remove_and_get() {
	mut ctx := gg.new_context(width: 100)
	image := gg.Image{
		ok: true
	}
	idx := ctx.cache_image(image)
	assert ctx.get_cached_image_by_idx(idx).ok == true
	ctx.remove_cached_image_by_idx(idx)
	assert ctx.get_cached_image_by_idx(idx).ok == false
}

fn test_new_context_sets_borderless_window_flag() {
	ctx := gg.new_context(
		width:             100
		borderless_window: true
	)
	assert ctx.window.borderless_window == true
}

fn test_create_image_from_byte_array_loads_rgba_pixels() {
	mut ctx := gg.new_context(width: 100)
	background_bytes := os.read_bytes(background_path)!
	img := ctx.create_image_from_byte_array(background_bytes)!
	assert img.width > 0
	assert img.height > 0
	assert img.nr_channels == 4
	assert !isnil(img.data)
	assert ctx.get_cached_image_by_idx(img.id).nr_channels == 4
}
