import os
import v.cmdexec

fn test_linux_gg_wayland_uses_the_new_compiler_and_32_bit_pipe_descriptors() {
	root := os.join_path(os.vtmp_dir(), 'sokol_wayland_codegen_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	saved_fallback := os.getenv('V_MACOS_V3_NO_FALLBACK')
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	defer { os.setenv('V_MACOS_V3_NO_FALLBACK', saved_fallback, true) }
	// The X11 control generates C only; it must not depend on the host session.
	saved_display := os.getenv('DISPLAY')
	os.setenv('DISPLAY', ':codegen', true)
	defer { os.setenv('DISPLAY', saved_display, true) }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import gg
fn main() {
 mut window := gg.new_context(width: 160, height: 120, window_title: "Wayland codegen")
 window.run()
}
')!
	for enabled in [false, true] {
		c_file := os.join_path(root, 'gg_${enabled}.c')
		mut args := ['-gc', 'none', '-os', 'linux', '-arch', 'amd64', '-dump-c-flags', '-']
		if enabled { args << ['-d', 'sokol_wayland'] }
		args << ['-o', c_file, source]
		result := cmdexec.run(@VEXE, args)
		assert result.exit_code == 0, result.output
		code := os.read_file(c_file)!
		if enabled {
			for library in ['-lwayland-client', '-lwayland-egl', '-lxkbcommon'] {
				assert library in result.output.split_into_lines(), result.output
			}
			assert code.contains('#define SOKOL_WAYLAND'), code[..1000]
			assert !code.contains('#define SOKOL_DISABLE_WAYLAND')
			pipe_body := code.all_after('sapp__wl_create_drop_pipe(void) {').all_before('\n}')
			assert pipe_body.contains('i32 fds[2];'), pipe_body
			assert pipe_body.contains('pipe(&fds[0])'), pipe_body
			assert !pipe_body.contains('i64 fds[2];'), pipe_body
		} else {
			assert '-lwayland-client' !in result.output.split_into_lines(), result.output
			assert code.contains('#define SOKOL_DISABLE_WAYLAND')
			assert !code.contains('#define SOKOL_WAYLAND')
			assert !code.contains('sapp__wl_create_drop_pipe(void) {')
		}
	}
}
