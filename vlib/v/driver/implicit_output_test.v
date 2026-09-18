module driver

import os
import time
import v.cmdexec

const implicit_output_source = "fn main() {\n\tprintln('IMPLICIT_OUTPUT_RAN')\n\texit(37)\n}\n"

fn implicit_output_test_root() !string {
	root := os.join_path(os.vtmp_dir(), 'v implicit output ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	return root
}

fn implicit_output_compile(args []string) os.Result {
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		} else {
			os.unsetenv('VFLAGS')
		}
		if value := old_vosargs {
			os.setenv('VOSARGS', value, true)
		} else {
			os.unsetenv('VOSARGS')
		}
	}
	vexe := if os.base(@VEXE) in ['v1_fallback', 'v1_fallback.exe'] {
		os.join_path(os.dir(@VEXE), 'v' + $if windows { '.exe' } $else { '' })
	} else {
		@VEXE
	}
	// Use the host C compiler, not a bundled TinyCC or a compatibility retry.
	cc := $if windows { 'gcc' } $else { 'cc' }
	mut command := ['-new-compiler', '-no-retry-compilation', '-nocache', '-cc', cc]
	command << args
	return cmdexec.run_with_timeout(vexe, command, 120_000)
}

fn implicit_output_binary(source string) string {
	return source + '.out' + $if windows { '.exe' } $else { '' }
}

fn test_default_output_for_non_v_suffixes_is_distinct_from_source() {
	root := implicit_output_test_root()!
	old_directory := os.getwd()
	os.chdir(root)!
	defer {
		os.chdir(old_directory) or { panic(err) }
		os.rmdir_all(root) or { panic(err) }
	}
	for name in ['script', 'tmp.abcdef', '.hidden', 'input.txt', 'source.exe', 'café script'] {
		assert default_bin_file_for_input(name) == name + '.out'
		input := os.join_path('source directory', name)
		assert default_bin_file_for_input(input) == input + '.out'
	}
	assert default_bin_file_for_input('unsafe\tinput') == 'unsafe_input.out'
	assert default_bin_file_for_input('unsafe\x7finput.tmp') == 'unsafe_input.tmp.out'
	// Keep the established names for normal V files and backend suffixes.
	for suffix in ['.v', '.vv', '.vsh', '.c.v', '.js.v', '.wasm.v'] {
		assert default_bin_file_for_input('ordinary' + suffix) == 'ordinary'
	}
	for name in ['.v', '.vv', '.vsh'] {
		assert default_bin_file_for_input(name) == name + '.out'
	}
}

fn test_default_output_resolves_extensionless_sources_and_symlinks_safely() {
	root := implicit_output_test_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(os.join_path(root, 'sources'))!
	os.mkdir_all(os.join_path(root, 'links'))!
	for name in ['script', 'tmp.abcdef', '.hidden'] {
		source := os.join_path(root, 'sources', name)
		os.write_file(source, implicit_output_source)!
		expected := os.real_path(source) + '.out'
		assert default_bin_file_for_input(source) == expected
		$if !windows {
			// Even a .v link can resolve to a target with no V suffix.
			link := os.join_path(root, 'links', name + '.v')
			os.symlink(source, link)!
			assert default_bin_file_for_input(link) == expected
		}
		assert os.read_file(source)! == implicit_output_source
	}
	assert default_bin_file_for_input(root) == os.join_path_single(os.real_path(root), os.file_name(root))
}

fn test_compiling_extensionless_input_preserves_source_and_existing_run_output() {
	root := implicit_output_test_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	source := os.join_path(root, 'compile me')
	output := implicit_output_binary(source)
	os.write_file(source, implicit_output_source)!
	build := implicit_output_compile([source])
	assert build.exit_code == 0, build.output
	assert os.read_file(source)! == implicit_output_source
	assert os.is_file(output), build.output
	run := cmdexec.run_with_timeout(output, [], 10_000)
	assert run.exit_code == 37, run.output
	assert run.output.trim_space() == 'IMPLICIT_OUTPUT_RAN'
	// `v run` retains an executable that existed before that invocation.
	rerun := implicit_output_compile(['run', source])
	assert rerun.exit_code == 37, rerun.output
	assert rerun.output.contains('IMPLICIT_OUTPUT_RAN'), rerun.output
	assert os.is_file(output)
	assert os.read_file(source)! == implicit_output_source
}

fn test_running_temporary_input_preserves_source_and_cleans_implicit_output() {
	root := implicit_output_test_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	// Match the mktemp-style input used by the Termux OS probe.
	source := os.join_path(root, 'tmp.abcdef')
	output := implicit_output_binary(source)
	os.write_file(source, implicit_output_source)!
	run := implicit_output_compile(['run', source])
	assert run.exit_code == 37, run.output
	assert run.output.contains('IMPLICIT_OUTPUT_RAN'), run.output
	assert os.read_file(source)! == implicit_output_source
	assert !os.exists(output)
	assert !os.exists(output + '.c')
}

fn test_explicit_output_and_failed_compilation_preserve_extensionless_source() {
	root := implicit_output_test_root()!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	source := os.join_path(root, 'script')
	output := os.join_path(root, 'chosen output' + $if windows { '.exe' } $else { '' })
	os.write_file(source, implicit_output_source)!
	run := implicit_output_compile(['-o', output, 'run', source])
	assert run.exit_code == 37, run.output
	assert run.output.contains('IMPLICIT_OUTPUT_RAN'), run.output
	assert os.is_file(output)
	assert !os.exists(implicit_output_binary(source))
	assert os.read_file(source)! == implicit_output_source

	bad_source := os.join_path(root, 'broken')
	bad_text := 'fn main() { unknown_implicit_output_function() }\n'
	os.write_file(bad_source, bad_text)!
	failed := implicit_output_compile([bad_source])
	assert failed.exit_code != 0, failed.output
	assert failed.output.contains('unknown_implicit_output_function'), failed.output
	assert os.read_file(bad_source)! == bad_text
	assert !os.exists(implicit_output_binary(bad_source))
}
