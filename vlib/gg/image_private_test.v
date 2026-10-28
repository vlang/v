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
