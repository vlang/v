module main

import os
import v.cmdexec
import v.pref

const native_backend_aliases = [
	['-native'],
	['-b', 'native'],
	['-backend', 'native'],
	['-b=native'],
	['-backend=native'],
]

fn native_backend_cli_can_run() bool {
	$if linux && amd64 {
		return true
	} $else $if macos && arm64 {
		return true
	} $else {
		return false
	}
}

fn run_native_backend_cli(args []string, root string, vflags string) os.Result {
	return cmdexec.run_in_merged_with_env(@VEXE, args, root, {
		'VFLAGS':                 vflags
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
}

fn test_native_backend_aliases_match_the_host_backend() {
	if !native_backend_cli_can_run() {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_backend_host_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println(@BACKEND) }\n')!
	expected := if pref.host_arch() == 'amd64' { 'amd64' } else { 'arm64' }
	explicit := run_native_backend_cli(['-silent', '-b', pref.host_arch(), 'run', source],
		root, '')
	assert explicit.exit_code == 0, explicit.output
	assert explicit.output.trim_space() == expected, explicit.output
	for alias in native_backend_aliases {
		mut args := ['-silent']
		args << alias
		args << ['run', source]
		result := run_native_backend_cli(args, root, '')
		assert result.exit_code == 0, '${args}: ${result.output}'
		assert result.output.trim_space() == expected, '${args}: ${result.output}'
	}
}

fn test_native_backend_aliases_use_the_last_selected_backend() {
	if !native_backend_cli_can_run() {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_backend_order_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println(@BACKEND) }\n')!
	expected := if pref.host_arch() == 'amd64' { 'amd64' } else { 'arm64' }
	for alias in native_backend_aliases {
		mut native_first := ['-silent']
		native_first << alias
		native_first << ['-backend', 'c', 'run', source]
		c_result := run_native_backend_cli(native_first, root, '')
		assert c_result.exit_code == 0, c_result.output
		assert c_result.output.trim_space() == 'c', c_result.output
		mut native_last := ['-silent', '-b', 'c']
		native_last << alias
		native_last << ['run', source]
		native_result := run_native_backend_cli(native_last, root, '')
		assert native_result.exit_code == 0, native_result.output
		assert native_result.output.trim_space() == expected, native_result.output
	}
}

fn test_native_backend_aliases_use_the_requested_architecture_in_either_order() {
	root := os.join_path(os.vtmp_dir(), 'native_backend_target_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {}\n')!
	target_arch := if pref.host_arch() == 'amd64' { 'arm64' } else { 'amd64' }
	explicit := run_native_backend_cli(['-silent', '-b', target_arch, '-os', 'linux', '-arch',
		target_arch, '-check', source], root, '')
	for alias in native_backend_aliases {
		mut arch_first := ['-silent', '-os', 'linux', '-arch', target_arch]
		arch_first << alias
		mut arch_last := ['-silent', '-os', 'linux']
		arch_last << alias
		arch_last << ['-arch', target_arch]
		for flags in [arch_first, arch_last] {
			mut args := flags.clone()
			args << ['-check', source]
			result := run_native_backend_cli(args, root, '')
			assert result.exit_code == explicit.exit_code, '${args}: ${result.output}'
			assert result.output == explicit.output, '${args}: ${result.output}'
		}
	}
}

fn test_native_backend_cross_compiles_arm64_objects_and_overrides_vflags_architecture() {
	$if !linux || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_backend_arm64_object_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println("hello") }\n')!
	for index, vflags in ['', '-native -arch amd64'] {
		output := os.join_path(root, 'main_${index}.o')
		args := if vflags == '' {
			['-silent', '-b', 'native', '-arch', 'arm64', '-o', output, source]
		} else {
			['-silent', '-arch', 'aarch64', '-o', output, source]
		}
		result := run_native_backend_cli(args, root, vflags)
		assert result.exit_code == 0, '${args}: ${result.output}'
		object := os.read_bytes(output)!
		assert object.len >= 64
		assert object[..4] == [u8(0x7f), `E`, `L`, `F`]
		assert object[4] == 2 // ELFCLASS64
		assert object[5] == 1 // ELFDATA2LSB
		assert object[16..18] == [u8(1), 0] // ET_REL
		assert object[18..20] == [u8(183), 0] // EM_AARCH64
	}
}

fn test_native_backend_aliases_reject_unsupported_requested_architectures() {
	for alias in native_backend_aliases {
		mut args := alias.clone()
		args << ['-arch', 'riscv64', '-check', os.join_path(@VMODROOT, 'examples', 'hello_world.v')]
		result := run_native_backend_cli(args, '', '')
		assert result.exit_code == 1, result.output
		assert result.output.contains('native backend is not supported on riscv64'), result.output
	}
}

fn test_native_backend_aliases_apply_to_vflags_and_test_programs() {
	if !native_backend_cli_can_run() {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_backend_vflags_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() { println(@BACKEND) }\n')!
	expected := if pref.host_arch() == 'amd64' { 'amd64' } else { 'arm64' }
	test_source := os.join_path(root, 'backend_test.v')
	os.write_file(test_source, "fn test_backend() { assert @BACKEND == '${expected}' }\n")!
	for vflags in ['-native', '-b native'] {
		result := run_native_backend_cli(['-silent', 'run', source], root, vflags)
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == expected, result.output
		overridden := run_native_backend_cli(['-silent', '-b', 'c', 'run', source], root,
			vflags)
		assert overridden.exit_code == 0, overridden.output
		assert overridden.output.trim_space() == 'c', overridden.output
		tested := run_native_backend_cli(['-silent', 'test', test_source], root, vflags)
		assert tested.exit_code == 0, tested.output
	}
}

fn test_native_backend_aliases_reject_unsupported_host_architectures() {
	if pref.host_arch() in ['amd64', 'arm64'] {
		return
	}
	for alias in native_backend_aliases {
		mut args := alias.clone()
		args << ['-check', os.join_path(@VMODROOT, 'examples', 'hello_world.v')]
		result := run_native_backend_cli(args, '', '')
		assert result.exit_code == 1, result.output
		assert result.output.contains('native backend is not supported on ${pref.host_arch()}'), result.output
	}
}

fn test_native_backend_aliases_run_native_suffixed_tests() {
	if !native_backend_cli_can_run() {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_backend_suffix_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'backend_test.native.v')
	expected := if pref.host_arch() == 'amd64' { 'amd64' } else { 'arm64' }
	os.write_file(source, "fn test_backend() {
	assert @BACKEND == '${expected}'
	println('NATIVE_SUFFIX_EXECUTED')
	assert false
}\n")!
	for alias in [['-native'], ['-b', 'native'], ['-b', pref.host_arch()]] {
		mut args := alias.clone()
		args << ['test', source]
		result := run_native_backend_cli(args, root, '')
		assert result.exit_code == 1, result.output
		assert result.output.contains('NATIVE_SUFFIX_EXECUTED'), result.output
	}
}

fn test_native_backend_vflags_are_forwarded_to_tests_once() {
	root := os.join_path(os.vtmp_dir(), 'native_backend_vflags_once_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'flags_test.v')
	os.write_file(source,
		"fn test_flags() { assert false, 'NATIVE_VFLAGS_EXECUTED' }\n")!
	cflags := '-DV_NATIVE_FLAGS_ONCE=1'
	for alias in ['-native', '-b native'] {
		vflags := '${alias} -cflags ${os.quoted_path(cflags)}'
		// Strict test mode merges VFLAGS only when the launcher has not already forwarded it.
		result := run_native_backend_cli(['-b', 'c', 'test', source], root, vflags)
		assert result.exit_code == 1, result.output
		assert result.output.contains('NATIVE_VFLAGS_EXECUTED'), result.output
		hints := result.output.split_into_lines().filter(it.contains('To reproduce just failure'))
		assert hints.len == 1, result.output
		forwarded := os.split_args(hints[0].all_after('run:').trim_space())!
		assert forwarded.filter(it == '-cflags').len == 1, hints[0]
		assert forwarded.filter(it == cflags).len == 1, hints[0]
	}
}

fn test_native_backend_aliases_allow_help_and_formatting() {
	missing := os.join_path(os.vtmp_dir(), 'missing_native_input_${os.getpid()}.v')
	for alias in native_backend_aliases {
		mut help_args := alias.clone()
		help_args << ['help', 'build-c']
		help_result := run_native_backend_cli(help_args, '', '')
		assert help_result.exit_code == 0, help_result.output
		mut fmt_args := alias.clone()
		fmt_args << ['fmt', missing]
		fmt_result := run_native_backend_cli(fmt_args, '', '')
		assert fmt_result.exit_code == 1, fmt_result.output
		assert fmt_result.output.contains('vfmt error:'), fmt_result.output
		assert fmt_result.output.contains('does not exist'), fmt_result.output
		assert !fmt_result.output.contains('has been replaced'), fmt_result.output
	}
}

fn test_native_backend_spelling_in_completion_arguments() {
	for alias in native_backend_aliases {
		mut args := ['complete', 'bash', 'v']
		args << alias
		result := run_native_backend_cli(args, '', '')
		assert result.exit_code == 0, result.output
		assert !result.output.contains('has been replaced'), result.output
	}
}

fn test_native_backend_spelling_in_program_arguments_and_option_values() {
	root := os.join_path(os.vtmp_dir(), 'native_backend_cli_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	script := os.join_path(root, 'main.vsh')
	code := 'import os\nfn main() { println(os.args[1..].join(" ")) }\n'
	os.write_file(source, code)!
	os.write_file(script, code)!
	program_args := ['-native', '-b', 'native', '-backend', 'native', '-b=native', '-backend=native']
	for input_args in [
		['run', source],
		['crun', source],
		[script],
		['-e', code],
		['-e=${code}'],
	] {
		mut args := ['-silent']
		args << input_args
		args << program_args
		result := run_native_backend_cli(args, root, '')
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == program_args.join(' '), result.output
	}
	for value in ['-native', '-b=native', '-backend=native', '-b native'] {
		result := run_native_backend_cli(['-silent', '-cflags', value, '-check', source],
			root, '')
		assert result.exit_code == 0, result.output
		assert !result.output.contains('has been replaced'), result.output
	}
}
