// vtest build: !musl?

import os

fn compile_probe(name string, code string) !os.Result {
	source_path := os.join_path(os.vtmp_dir(), 'sapp_screenshot_${name}_${os.getpid()}.v')
	output_path := os.join_path(os.vtmp_dir(), 'sapp_screenshot_${name}_${os.getpid()}.c')
	defer {
		os.rm(source_path) or {}
		os.rm(output_path) or {}
	}
	os.write_file(source_path, code)!
	return os.exec([@VEXE, '-b', 'c', '-o', output_path, source_path])
}

fn test_screenshot_fields_and_pixels_are_readable_outside_sapp() {
	result := compile_probe('read', 'import sokol.sapp

fn inspect(ss &sapp.Screenshot) (int, int, int, &u8) {
	return ss.width, ss.height, ss.size, ss.pixels()
}

fn main() {
	mut ss := sapp.screenshot_window()
	_, _, _, _ := inspect(ss)
	unsafe { ss.destroy() }
}
')!
	assert result.exit_code == 0, result.output
}

fn test_screenshot_pixels_field_stays_private_outside_sapp() {
	result := compile_probe('write', 'import sokol.sapp

fn main() {
	mut ss := sapp.screenshot_window()
	ss.pixels = unsafe { nil }
}
')!
	assert result.exit_code != 0, result.output
	assert result.output.contains('`sokol.sapp.Screenshot.pixels` is not public'), result.output
}
