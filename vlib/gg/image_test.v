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

fn test_remove_cached_images_by_id_keeps_other_ids_valid() {
	mut ctx := gg.new_context(width: 100)
	background_bytes := os.read_bytes(background_path)!
	prev := ctx.create_image_from_byte_array(background_bytes)!
	curr := ctx.create_image_from_byte_array(background_bytes)!
	assert prev.id == 0
	assert curr.id == 1
	assert ctx.get_cached_image_by_idx(prev.id).ok == true
	assert ctx.get_cached_image_by_idx(curr.id).ok == true
	ctx.remove_cached_image_by_idx(prev.id)
	ctx.remove_cached_image_by_idx(curr.id)
	assert ctx.get_cached_image_by_idx(prev.id).ok == false
	assert ctx.get_cached_image_by_idx(curr.id).ok == false
}

fn test_new_context_sets_borderless_window_flag() {
	ctx := gg.new_context(
		width:             100
		borderless_window: true
	)
	assert ctx.window.borderless_window == true
}

fn test_new_context_sets_texture_filter() {
	ctx := gg.new_context(
		width:          100
		texture_filter: .nearest
	)
	assert ctx.config.texture_filter == .nearest
}

fn test_create_image_from_byte_array_loads_rgba_pixels() {
	mut ctx := gg.new_context(width: 100)
	background_bytes := os.read_bytes(background_path)!
	img := ctx.create_image_from_byte_array(background_bytes)!
	assert img.width > 0
	assert img.height > 0
	assert img.nr_channels == 4
	assert !isnil(img.data)
	assert img.texture_filter == .linear
	assert ctx.get_cached_image_by_idx(img.id).nr_channels == 4
}

fn test_create_image_from_byte_array_uses_context_texture_filter() {
	mut ctx := gg.new_context(
		width:          100
		texture_filter: .nearest
	)
	background_bytes := os.read_bytes(background_path)!
	img := ctx.create_image_from_byte_array(background_bytes)!
	assert img.texture_filter == .nearest
	assert ctx.get_cached_image_by_idx(img.id).texture_filter == .nearest
}

fn test_create_image_from_byte_array_with_filter_overrides_context_default() {
	mut ctx := gg.new_context(width: 100)
	background_bytes := os.read_bytes(background_path)!
	img := ctx.create_image_from_byte_array_with_filter(background_bytes, .nearest)!
	assert img.texture_filter == .nearest
	assert ctx.get_cached_image_by_idx(img.id).texture_filter == .nearest
}
