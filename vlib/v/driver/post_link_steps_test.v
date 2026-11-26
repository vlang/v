module driver

import os
import v.cmdexec

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

// shown_path is `path` the way a step line shows it: every argument of a step goes through
// cmdexec.display, which quotes one that has a space or another character a shell reads.
fn shown_path(path string) string {
	return cmdexec.display('mv', [path]).all_after('mv ')
}

// move_lines_to returns the lines of a build's output that show a move to `target`.
fn move_lines_to(output string, target string) []string {
	return output.split_into_lines().filter(it.starts_with('  > mv ')
		&& it.ends_with(' ' + shown_path(target)))
}

fn test_a_path_is_matched_the_way_it_is_shown() {
	assert shown_path('/t/p') == '/t/p'
	spaced := shown_path('/t dir/p')
	assert spaced != '/t dir/p' && spaced.contains('/t dir/p'), spaced
	output := '  > cc -o out src.c\n  > mv ${shown_path('/t dir/.p.tmp')} ${spaced}\n'
	assert move_lines_to(output, '/t dir/p').len == 1
	assert move_lines_to(output, '/t dir/other').len == 0
}

// fastc_unavailable says why this compiler or this host cannot build with FastC, or '' when the
// failure is something else. A compiler built with `-d skip_fastc` has no FastC, and the TinyCC
// path needs the bundled TinyCC.
fn fastc_unavailable(output string) string {
	if output.contains('fastc support is not compiled into this v3 executable') {
		return 'FastC is not compiled into this compiler'
	}
	if output.contains('fastc requires the bundled TinyCC executable') {
		return 'the bundled TinyCC is not here'
	}
	return ''
}

// Every path that puts the output in place reports its move: the C backend, FastC with TinyCC and
// FastC with Clang each return their own result, and the Clang one used to leave the step out.
fn test_showcc_prints_the_move_that_puts_the_output_in_place() {
	// The directory has a space in its name, so the move is shown with quoted paths; a plain
	// path is checked at the end. The temporary directory itself may have a space as well.
	root := os.join_path(os.vtmp_dir(), 'post link steps ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("linked") }')!
	mut executable := os.join_path(root, 'program')
	$if windows {
		executable += '.exe'
	}
	assert shown_path(executable) != executable, 'the path has nothing to quote: ${executable}'
	mut toolchains := [[]string{}]
	$if !windows {
		toolchains << ['-b', 'fastc']
		if os.exists_in_system_path('clang') {
			toolchains << ['-b', 'fastc', '-cc', 'clang']
		} else {
			println('not run: -b fastc -cc clang (no clang on PATH)')
		}
	}
	mut ran := []string{}
	for toolchain in toolchains {
		name := if toolchain.len == 0 { 'the C backend' } else { toolchain.join(' ') }
		for flag in ['-showcc', '-v'] {
			os.rm(executable) or {}
			result := os.exec([@VEXE, ...toolchain, flag, '-gc', 'none', '-nocache', '-o', executable,
				source])
			if result.exit_code != 0 && toolchain.len > 0 {
				why := fastc_unavailable(result.output)
				if why != '' {
					println('not run: ${name} (${why})')
					break
				}
			}
			assert result.exit_code == 0, '${name} ${flag}:\n${result.output}'
			lines := result.output.split_into_lines()
			mv_lines := move_lines_to(result.output, executable)
			assert mv_lines.len == 1, '${name} ${flag}: expected one move line, got ${mv_lines}\n${result.output}'
			// It comes after the line of the compiler that made the file.
			compile_at := lines.index(lines.filter(it.starts_with('  > ') && !it.starts_with('  > mv '))[0])
			assert lines.index(mv_lines[0]) > compile_at, '${name} ${flag}'
			ran << '${name} ${flag}'
		}
	}
	// The C backend is never passed over, so a run in which nothing was checked cannot pass.
	assert 'the C backend -showcc' in ran && 'the C backend -v' in ran, ran.str()
	println('checked: ${ran.join(', ')}')
	// A path with nothing to quote is shown as it is.
	plain_root := os.join_path(root, 'plain')
	os.mkdir_all(plain_root)!
	plain_source := os.join_path(plain_root, 'main.v')
	os.write_file(plain_source, 'fn main() { println("linked") }')!
	working_dir := os.getwd()
	os.chdir(plain_root)!
	plain_name := $if windows { 'plain_program.exe' } $else { 'plain_program' }
	plain := os.exec([@VEXE, '-showcc', '-gc', 'none', '-nocache', '-o', plain_name, 'main.v'])
	os.chdir(working_dir)!
	assert plain.exit_code == 0, plain.output
	plain_moves := plain.output.split_into_lines().filter(it.starts_with('  > mv ')
		&& it.ends_with(' ${plain_name}'))
	assert plain_moves.len == 1, plain.output
	// Without the flags the build says nothing of it.
	os.rm(executable) or {}
	quiet := os.exec([@VEXE, '-gc', 'none', '-nocache', '-o', executable, source])
	assert quiet.exit_code == 0, quiet.output
	assert !quiet.output.contains('  > '), quiet.output
}
