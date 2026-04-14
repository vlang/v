module gfx

import os

fn assert_program_panics(source string, expected string) {
	source_path := os.join_path(os.temp_dir(),
		'sokol_gfx_make_image_requires_setup_${os.getpid()}.v')
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
	}
	res := os.execute('${os.quoted_path(@VEXE)} run ${os.quoted_path(source_path)}')
	assert res.exit_code != 0, 'expected the test program to fail'
	assert res.output.contains(expected), 'expected `${expected}` in `${res.output}`'
}

fn test_make_image_requires_setup() {
	assert_program_panics('import sokol.gfx

fn main() {
	mut desc := gfx.ImageDesc{}
	_ = gfx.make_image(&desc)
}
',
		'sokol.gfx is not initialized; call gfx.setup(...) before gfx.make_image(...)')
}
