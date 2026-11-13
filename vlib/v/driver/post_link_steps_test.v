module driver

import os

// The steps the driver takes after the link are shown with `-showcc` and `-v`, with the same
// `  > ` prefix as the compiler command line.

fn test_step_lines_use_the_prefix_of_the_compiler_command() {
	assert post_link_step_line('mv a b') == '  > mv a b'
}

fn test_the_move_step_names_both_files() {
	assert post_link_move_step('/t/.p.tmp', '/t/p') == 'mv /t/.p.tmp /t/p'
}

fn test_macos_steps_are_the_commands_that_are_run() {
	assert macos_codesign_step('/t/p') == 'codesign -f -s - /t/p'
	assert macos_in_process_codesign_step('/t/p').contains('codesign -f -s - /t/p')
	assert macos_dsym_step('/usr/bin/dsymutil', '/t/p', '/t/p.dSYM') == '/usr/bin/dsymutil /t/p -o /t/p.dSYM'
	assert macos_dsym_move_step('/t/.p.dSYM', '/t/p.dSYM') == 'mv /t/.p.dSYM /t/p.dSYM'
}

fn test_windows_manifest_step_names_the_sidecar_and_the_action() {
	assert windows_manifest_step('C:/t/p.exe', false).starts_with('write C:/t/p.exe.manifest')
	assert windows_manifest_step('C:/t/p.exe', true).starts_with('rm C:/t/p.exe.manifest')
}

fn test_showcc_prints_the_move_that_puts_the_output_in_place() {
	root := os.join_path(os.vtmp_dir(), 'post_link_steps_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("linked") }')!
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	for flag in ['-showcc', '-v'] {
		os.rm(executable) or {}
		result := os.exec([@VEXE, '-new-compiler', flag, '-gc', 'none', '-nocache', '-o', executable,
			source])
		assert result.exit_code == 0, result.output
		lines := result.output.split_into_lines()
		mv_lines := lines.filter(it.starts_with('  > ') && it.contains(' ${executable}')
			&& it.contains('mv '))
		assert mv_lines.len == 1, '${flag}: expected one move line, got ${mv_lines}\n${result.output}'
		// It comes after the line of the compiler that made the file.
		compile_at := lines.index(lines.filter(it.starts_with('  > ') && !it.contains('mv '))[0])
		assert lines.index(mv_lines[0]) > compile_at
	}
	// Without the flags the build says nothing of it.
	os.rm(executable) or {}
	quiet := os.exec([@VEXE, '-new-compiler', '-gc', 'none', '-nocache', '-o', executable, source])
	assert quiet.exit_code == 0, quiet.output
	assert !quiet.output.contains('  > '), quiet.output
}
