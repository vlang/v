// vtest build: !docker-ubuntu-musl // needs GL/gl.h
module gg

import os
import sokol.gfx

const background_path = os.join_path(@VEXEROOT, 'examples/flappylearning/assets/img/background.png')

fn test_load_image_loads_supported_file_contents() {
	img := load_image(background_path)!
	assert img.width > 0
	assert img.height > 0
	assert img.nr_channels == 4
	assert img.path == background_path
}

fn test_load_image_returns_error_for_unsupported_file_contents() {
	file := os.join_path(os.vtmp_dir(), 'gg_unsupported_image_${os.getpid()}.jpg')
	os.write_file(file, 'not an image') or { panic(err) }
	defer {
		os.rm(file) or {}
	}
	load_image(file) or {
		assert err.msg().contains('stbi_image failed to load')
		return
	}
	assert false
}

fn test_sokol_mipmap_counts_base_level() {
	width := 64
	height := 64
	mut base := unsafe { malloc(width * height * 4) }
	defer {
		unsafe { free(base) }
	}
	mut desc := gfx.ImageDesc{
		width:  width
		height: height
	}
	desc.data.subimage[0][0] = gfx.Range{
		ptr:  base
		size: uint(width * height * 4)
	}
	mip_data := sokol_mipmap(mut desc, 0)
	defer {
		if mip_data != unsafe { nil } {
			unsafe { free(mip_data) }
		}
	}
	// 64 -> 32 -> 16 -> 8 -> 4 -> 2 -> 1: base level + 6 generated levels
	assert desc.num_mipmaps == 7
	assert desc.data.subimage[0][6].size == uint(1 * 1 * 4)
	assert desc.data.subimage[0][6].ptr != unsafe { nil }
}

fn test_sokol_mipmap_tiny_image_needs_no_alloc() {
	mut desc := gfx.ImageDesc{
		width:  1
		height: 1
	}
	mip_data := sokol_mipmap(mut desc, 1)
	assert desc.num_mipmaps == 1
	assert mip_data == unsafe { nil }
}

fn test_sokol_mipmap_pixels_and_level_cap() {
	mut pixels := []u8{len: 4 * 4 * 4}
	for y in 0 .. 4 {
		for x in 0 .. 4 {
			offset := (y * 4 + x) * 4
			pixels[offset] = u8((y * 4 + x) * 4)
			pixels[offset + 1] = u8(x * 8)
			pixels[offset + 2] = u8(y * 8)
			pixels[offset + 3] = 255
		}
	}
	expected_half := [u8(10), 4, 4, 255, 18, 20, 4, 255, 42, 4, 20, 255, 50, 20, 20, 255]
	expected_last := [u8(30), 12, 12, 255]
	for cap in [1, 2, 3, 0, gfx.sg_max_mipmaps + 1] {
		mut desc := gfx.ImageDesc{
			width:  4
			height: 4
		}
		desc.data.subimage[0][0] = gfx.Range{
			ptr:  pixels.data
			size: uint(pixels.len)
		}
		buffer := sokol_mipmap(mut desc, cap)
		defer {
			if !isnil(buffer) {
				unsafe { free(buffer) }
			}
		}
		expected_levels := if cap == 1 {
			1
		} else if cap == 2 {
			2
		} else {
			3
		}
		assert desc.num_mipmaps == expected_levels
		assert desc.data.subimage[0][0].ptr == pixels.data
		if cap == 1 {
			assert isnil(buffer)
		} else {
			half := desc.data.subimage[0][1]
			assert half.ptr == buffer
			assert half.size == uint(expected_half.len)
			// The descriptor owns a view into the live allocation until it is freed above.
			assert unsafe { (&u8(half.ptr)).vbytes(int(half.size)) } == expected_half
		}
		if expected_levels == 3 {
			last := desc.data.subimage[0][2]
			assert last.ptr == unsafe { &u8(buffer) + expected_half.len }
			assert last.size == uint(expected_last.len)
			assert unsafe { (&u8(last.ptr)).vbytes(int(last.size)) } == expected_last
		} else {
			assert isnil(desc.data.subimage[0][expected_levels].ptr)
		}
	}
}

fn test_image_from_draw_config_accepts_first_cache_slot_and_checks_bounds() {
	mut ctx := new_context(width: 100)
	assert ctx.cache_image(Image{ width: 4, height: 4 }) == 0
	first := ctx.image_from_draw_config(DrawImageConfig{ img_id: 0 }) or {
		panic('cache index zero must be drawable')
	}
	assert first == &ctx.image_cache[0]
	assert first.width == 4
	for index in [-1, ctx.image_cache.len] {
		if _ := ctx.image_from_draw_config(DrawImageConfig{ img_id: index }) {
			assert false, 'out of bounds index must be rejected'
		}
	}
	provided := &Image{ width: 8 }
	selected := ctx.image_from_draw_config(DrawImageConfig{ img: provided, img_id: -1 }) or {
		panic('explicit image must take precedence')
	}
	assert selected == provided
}
