module x64

import os
import v2.mir

struct X64BackendCompileFailure {
	stdout    string
	stderr    string
	exit_code int
}

fn run_x64_backend_compile_failure(name string, source string) X64BackendCompileFailure {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_x64_backend_diagnostics_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := os.join_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	vexe := x64_backend_diagnostic_vexe_command_path()
	mut build := os.new_process(vexe)
	defer {
		build.close()
	}
	build.set_args(['-v2', '-b', 'x64', source_path, '-o', bin_path])
	build.set_redirect_stdio()
	build.run()
	build.wait()
	stdout := x64_backend_diagnostic_stdout(build.stdout_slurp())
	stderr := build.stderr_slurp()
	return X64BackendCompileFailure{
		stdout:    stdout
		stderr:    stderr
		exit_code: build.code
	}
}

fn x64_backend_diagnostic_vexe_command_path() string {
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	if os.is_abs_path(vexe) || !os.exists(vexe) {
		return vexe
	}
	return os.abs_path(vexe)
}

fn x64_backend_diagnostic_stdout(stdout string) string {
	mut lines := []string{}
	for line in stdout.split_into_lines() {
		if line.starts_with(' * ') {
			continue
		}
		lines << line
	}
	return lines.join('\n')
}

fn assert_x64_user_visible_compile_failure(failure X64BackendCompileFailure, expected_message string) {
	assert failure.exit_code != 0, failure.stdout + failure.stderr
	assert failure.stdout == '', failure.stdout
	assert failure.stderr.contains(expected_message), failure.stderr
	assert failure.stderr.contains(x64_backend_limitation_hint), failure.stderr
	assert !failure.stderr.contains('Link failed:'), failure.stderr
	assert !failure.stderr.contains('V panic:'), failure.stderr
	assert !failure.stderr.contains('Backtrace'), failure.stderr
	assert_x64_message_avoids_old_wording(failure.stderr)
}

fn assert_x64_clean_user_diagnostic_message(msg string) {
	assert msg.starts_with('x64: unsupported backend feature: '), msg
	assert !msg.contains('Link failed:'), msg
	assert !msg.contains('V panic:'), msg
	assert !msg.contains('Backtrace'), msg
	assert_x64_message_avoids_old_wording(msg)
}

fn assert_x64_message_avoids_old_wording(msg string) {
	assert !msg.contains('reachable helper'), msg
	assert !msg.contains('stdout/stderr path'), msg
	assert !msg.contains('must be compiled'), msg
	assert !msg.contains('lowered before linking'), msg
	assert !msg.contains('before external linking'), msg
	assert !msg.contains('cannot resolve C stdio symbol'), msg
	assert !msg.contains('C FILE streams'), msg
}

fn assert_x64_message_mentions_only_target_linker(format ObjectFormat, msg string) {
	match format {
		.elf {
			assert msg.contains('ELF linker'), msg
			assert !msg.contains('Mach-O'), msg
			assert !msg.contains('PE linker'), msg
			assert !msg.contains('Windows'), msg
			assert !msg.contains('Kernel32'), msg
			assert !msg.contains('macOS'), msg
		}
		.macho {
			assert msg.contains('Mach-O linker'), msg
			assert !msg.contains('ELF linker'), msg
			assert !msg.contains('PE linker'), msg
			assert !msg.contains('Windows'), msg
			assert !msg.contains('Kernel32'), msg
			assert !msg.contains('Linux'), msg
		}
		.coff {
			assert msg.contains('PE linker'), msg
			assert !msg.contains('ELF linker'), msg
			assert !msg.contains('Mach-O'), msg
			assert !msg.contains('Linux'), msg
			assert !msg.contains('macOS'), msg
		}
	}
}

fn test_x64_user_visible_stderr_reports_clean_codegen_abi_unsupported_without_link_noise() {
	$if windows {
		println('skipping ${@FN}: SysV codegen diagnostic is not available on Windows')
	} $else $if x64 {
		failure := run_x64_backend_compile_failure('clean_codegen_abi_unsupported', 'module main

fn many(a0 f64, a1 f64, a2 f64, a3 f64, a4 f64, a5 f64, a6 f64, a7 f64, a8 f64) f64 {
	return a8
}

fn main() {
	_ = many(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
}
')
		assert_x64_user_visible_compile_failure(failure,
			'x64: unsupported backend feature: stack-passed float parameter')
		assert !failure.stderr.contains('Windows'), failure.stderr
		assert !failure.stderr.contains('Kernel32'), failure.stderr
		assert !failure.stderr.contains('Mach-O'), failure.stderr
		assert !failure.stderr.contains('PE linker'), failure.stderr
	}
}

fn test_x64_user_visible_stderr_reports_captured_fn_literal_unsupported() {
	$if x64 {
		failure := run_x64_backend_compile_failure('captured_fn_literal_unsupported', 'module main

fn make_delta(delta int) fn (int) int {
	return fn [delta] (value int) int {
		return value + delta
	}
}

fn main() {
	f := make_delta(10)
	println(f(1))
}
')
		assert_x64_user_visible_compile_failure(failure, 'x64: unsupported backend feature: ')
		assert failure.stderr.contains('native x64 cannot lower captured function literal'), failure.stderr

		assert failure.stderr.contains('closure environments are not implemented yet'), failure.stderr
	}
}

fn test_x64_unsupported_backend_feature_message_is_normalized() {
	assert x64_unsupported_backend_feature_message('stack-passed float parameter') == 'x64: unsupported backend feature: stack-passed float parameter'
	assert x64_unsupported_backend_feature_message('backend feature: Windows argument lowering') == 'x64: unsupported backend feature: Windows argument lowering'
	assert x64_unsupported_backend_feature_message('backend feature: SysV direct aggregate call result with MEMORY eightbyte classes is not implemented yet') == 'x64: unsupported backend feature: SysV direct aggregate call result with MEMORY eightbyte classes is not implemented yet'
}

fn test_x64_user_visible_linker_diagnostic_message_for_generic_external_by_format() {
	for format in [ObjectFormat.elf, .macho, .coff] {
		msg := x64_unresolved_external_symbol_message(format, 'v_missing_runtime_symbol',
			'referenced from test relocation')

		assert_x64_clean_user_diagnostic_message(msg)
		assert_x64_message_mentions_only_target_linker(format, msg)
		assert msg.contains('cannot resolve external symbol `v_missing_runtime_symbol` yet'), msg
		assert msg.contains('referenced from test relocation'), msg
		assert !msg.contains('C stdio/file-descriptor symbol'), msg
		assert !msg.contains('Windows'), msg
		assert !msg.contains('Kernel32'), msg
		assert !msg.contains('C FILE/stdio'), msg
	}
}

fn test_x64_user_visible_crt_stdio_and_fd_diagnostic_is_windows_only() {
	for symbol_name in ['stderr', 'fread', '_get_osfhandle', '_open_osfhandle'] {
		for format in [ObjectFormat.elf, .macho] {
			msg := x64_unresolved_external_symbol_message(format, symbol_name,
				'referenced from test relocation')

			assert_x64_clean_user_diagnostic_message(msg)
			assert_x64_message_mentions_only_target_linker(format, msg)
			assert msg.contains('cannot resolve external symbol `${symbol_name}` yet'), msg
			assert !msg.contains('C stdio/file-descriptor symbol'), msg
			assert !msg.contains('Kernel32'), msg
			assert !msg.contains('C FILE/stdio'), msg
		}

		pe_msg := x64_unresolved_external_symbol_message(.coff, symbol_name,
			'referenced from test relocation')
		assert_x64_clean_user_diagnostic_message(pe_msg)
		assert_x64_message_mentions_only_target_linker(.coff, pe_msg)
		assert pe_msg.contains('cannot resolve C stdio/file-descriptor symbol `${symbol_name}`'), pe_msg
		assert pe_msg.contains('Windows x64 native backend uses Kernel32 handles'), pe_msg
		assert pe_msg.contains('Kernel32 handles'), pe_msg
		assert pe_msg.contains('C FILE/stdio calls'), pe_msg
		assert !pe_msg.contains('Linux'), pe_msg
		assert !pe_msg.contains('macOS'), pe_msg
	}
}

fn test_x64_user_visible_linker_diagnostic_message_for_missing_runtime_helper_by_format() {
	for format in [ObjectFormat.elf, .macho, .coff] {
		msg := x64_unresolved_external_symbol_message(format, 'builtin__Map_string_int__keys',
			'referenced from test relocation')

		assert_x64_clean_user_diagnostic_message(msg)
		assert_x64_message_mentions_only_target_linker(format, msg)
		assert msg.contains('cannot resolve V runtime helper `builtin__Map_string_int__keys`'), msg
		assert msg.contains('native x64 backend does not implement this feature for this target yet'), msg
		assert msg.contains('referenced from test relocation'), msg
		assert !msg.contains('C stdio/file-descriptor symbol'), msg
		assert !msg.contains('Kernel32'), msg
		assert !msg.contains('C FILE/stdio'), msg
		assert !msg.contains('not imported'), msg
	}
}

fn test_x64_gen_detects_missing_runtime_helper_while_preparing_output() {
	for format in [ObjectFormat.elf, .macho, .coff] {
		mut mod := mir.Module{}
		mut gen := Gen.new_with_format(&mod, format)
		match format {
			.elf {
				gen.elf.add_undefined('builtin__Map_string_int__keys')
			}
			.macho {
				gen.macho.add_undefined('_builtin__Map_string_int__keys')
			}
			.coff {
				gen.coff.add_undefined('builtin__Map_string_int__keys')
			}
		}

		msg := gen.unsupported_external_symbol_message() or {
			assert false
			continue
		}
		assert_x64_clean_user_diagnostic_message(msg)
		assert_x64_message_mentions_only_target_linker(format, msg)
		assert msg.contains(x64_linker_name(format)), msg
		assert msg.contains('builtin__Map_string_int__keys'), msg
		assert msg.contains('cannot resolve V runtime helper `builtin__Map_string_int__keys`'), msg
		assert msg.contains('native x64 backend does not implement this feature for this target yet'), msg
		assert msg.contains('needed while preparing native x64 output'), msg
	}
}

fn test_x64_gen_keeps_linker_resolved_external_symbols_for_external_linkers() {
	for format in [ObjectFormat.elf, .macho, .coff] {
		mut mod := mir.Module{}
		mut gen := Gen.new_with_format(&mod, format)
		match format {
			.elf {
				gen.elf.add_undefined('calloc')
			}
			.macho {
				gen.macho.add_undefined('_calloc')
			}
			.coff {
				gen.coff.add_undefined('calloc')
			}
		}

		if msg := gen.unsupported_external_symbol_message() {
			assert false, msg
		}
	}
}
