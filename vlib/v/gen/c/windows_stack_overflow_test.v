module c

import os
import v.pref

fn test_windows_spawn_wrapper_reserves_stack_before_the_user_call() {
	mut g := FlatGen.new()
	g.set_target(pref.target_from('windows', 'amd64') or { panic(err) })
	g.has_builtins = true
	body := g.spawn_wrapper_body('recurse()', 'void', '')
	assert body.starts_with('v_windows_set_stack_guarantee(); recurse();'), body
	g.compile_defines << 'no_backtrace'
	assert g.spawn_wrapper_body('recurse()', 'void', '').contains('v_windows_set_stack_guarantee();')
	g.compile_defines << 'no_segfault_handler'
	assert !g.spawn_wrapper_body('recurse()', 'void', '').contains('v_windows_set_stack_guarantee();')
}

fn test_windows_stack_overflow_header_and_spawned_program_compile() ! {
	dir := os.join_path(os.vtmp_dir(), 'windows_stack_overflow_codegen_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.v')
	c_source := os.join_path(dir, 'main.c')
	os.write_file(source, 'fn recurse(n int) int { if n == 0 { return 0 }; return 1 + recurse(n - 1) }
fn main() { worker := spawn recurse(10000000); println(worker.wait()) }
')!
	for enabled in [true, false] {
		mut flags := [@VEXE, '-new-compiler', '-nocache', '-os', 'windows', '-gc', 'none']
		if !enabled {
			flags << ['-d', 'no_segfault_handler']
		}
		flags << ['-o', c_source, source]
		generated_result := os.exec(flags)
		assert generated_result.exit_code == 0, generated_result.output
		generated := os.read_file(c_source)!
		assert generated.contains('v_windows_set_stack_guarantee();') == enabled
		assert generated.contains('segfault_handler_windows.h') == enabled
		assert generated.contains('v_install_windows_stack_overflow_handler();') == enabled
		cc_name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
		if cc := os.find_abs_path_of_executable(cc_name) {
			// The complete runtime has existing MinGW warnings unrelated to exception
			// handling; check the new header separately with warnings as errors.
			header := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'segfault_handler_windows.h')
			checked_header := os.exec([cc, '-Werror', '-Wall', '-fsyntax-only', '-x', 'c', header])
			assert checked_header.exit_code == 0, checked_header.output
			compiled := os.exec([cc, '-fsyntax-only', c_source])
			assert compiled.exit_code == 0, compiled.output
		}
	}
}
