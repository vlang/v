// vtest build: !docker-ubuntu-musl // needs GL/gl.h
module gg

import os

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
