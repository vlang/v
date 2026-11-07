module main

import os
import time
import v.cmdexec

struct VvmrcCommandResult {
	exit_code int
	output    string
}

fn run_vvmrc_command(folder string, args []string, envs map[string]string) VvmrcCommandResult {
	mut process := os.new_process(@VEXE)
	process.set_work_folder(folder)
	process.set_args(args)
	process.set_environment(envs)
	process.use_pgroup = true
	process.set_stdin_path(if os.user_os() == 'windows' { 'NUL' } else { '/dev/null' })
	process.set_redirect_stdio()
	process.run()
	watch := time.new_stopwatch()
	for process.is_alive() {
		if watch.elapsed().milliseconds() > 30_000 {
			process.signal_pgkill()
			break
		}
		time.sleep(10 * time.millisecond)
	}
	process.wait()
	result := VvmrcCommandResult{
		exit_code: process.code
		output:    process.stdout_slurp() + process.stderr_slurp()
	}
	process.close()
	return result
}

fn test_project_pin_selects_the_compiler_before_dispatching_tools() {
	root := os.join_path(os.vtmp_dir(), 'vvmrc_cli_${os.getpid()}')
	project := os.join_path(root, 'project')
	subdir := os.join_path(project, 'sub')
	bin := os.join_path(root, 'bin')
	os.mkdir_all(subdir)!
	os.mkdir_all(bin)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(project, 'main.v'), 'fn main() {}\n')!
	os.write_file(os.join_path(subdir, 'main.v'), 'fn main() {}\n')!
	os.write_file(os.join_path(project, '.vvmrc'), '99.99.99\n')!
	source := os.join_path(root, 'fake.v')
	os.write_file(source, 'import os\nfn main() {\n' +
		"println('PINNED ' + os.args[1..].join('|'))\n" +
		"println('SKIP=' + os.getenv('V_SKIP_VVMRC'))\nexit(23)\n}\n")!
	fake := os.join_path(bin, if os.user_os() == 'windows' {
		'v99.99.99.exe'
	} else {
		'v99.99.99'
	})
	built := cmdexec.run_with_timeout(@VEXE, ['-cc', @CCOMPILER, '-o', fake, source], 120_000)
	assert built.exit_code == 0, built.output
	mut envs := os.environ()
	path_key := if 'PATH' in envs { 'PATH' } else { 'Path' }
	envs[path_key] = bin + os.path_delimiter + envs[path_key]
	envs['V_SKIP_VVMRC'] = ''
	envs['VFLAGS'] = ''
	for args in [
		['fmt', '-verify', 'main.v'],
		['-cc', 'clang', 'fmt', '-verify', 'main.v'],
		['-o', root, 'run', 'main.v', root],
		['vet', 'main.v'],
		['doc', 'main.v'],
		['test', '.'],
		['run', 'sub'],
		['install', '--help'],
		['watch', 'run', 'main.v'],
		[]string{},
	] {
		result := run_vvmrc_command(project, args, envs)
		assert result.exit_code == 23, '${args}: ${result.output}'
		assert result.output.contains('PINNED ' + args.join('|')), result.output
		assert result.output.contains('SKIP=1'), result.output
	}
	from_subdir := run_vvmrc_command(subdir, ['fmt', '-verify', 'main.v'], envs)
	assert from_subdir.exit_code == 23, from_subdir.output
	for args in [
		['fmt', '-verify', os.join_path(project, 'main.v')],
		['doc', '-f', 'html', '-theme-dir', root, os.join_path(project, 'main.v')],
		['watch', '--before', 'echo before', '--add', root, 'run', os.join_path(project,
			'main.v')],
	] {
		from_outside := run_vvmrc_command(root, args, envs)
		assert from_outside.exit_code == 23, from_outside.output
	}
	// Stdin ends the compiler arguments; its program arguments are outside the project.
	stdin := run_vvmrc_command(project, ['run', '-', source], envs)
	assert stdin.exit_code == 23, stdin.output
	quiet := run_vvmrc_command(project, ['run', 'main.v', '-v'], envs)
	assert quiet.exit_code == 23, quiet.output
	assert !quiet.output.contains('selected V'), quiet.output
	for name in ['run', 'build', 'crun', 'bash', 'graph', 'VROOT'] {
		named_target := os.join_path(project, name)
		os.mkdir(named_target)!
		os.write_file(os.join_path(named_target, '.vvmrc'), '98.98.98\n')!
		for args in [['run', name], ['fmt', name]] {
			result := run_vvmrc_command(project, args, envs)
			assert result.exit_code == 1, result.output
			assert result.output.contains('pins V 98.98.98'), result.output
		}
	}
	bare_build := run_vvmrc_command(project, ['build'], envs)
	assert bare_build.exit_code == 23, bare_build.output
	for args in [['help', 'build'], ['--help', 'build'], ['complete', 'bash', 'v', '-cc'],
		['mod', 'graph'], ['env', 'VROOT']] {
		result := run_vvmrc_command(project, args, envs)
		assert result.exit_code == 23, result.output
	}
	// A nearer pin wins. It is missing, so even an installed parent pin must not run.
	os.write_file(os.join_path(subdir, '.vvmrc'), '98.98.98\n')!
	nested := run_vvmrc_command(subdir, ['fmt', '-verify', 'main.v'], envs)
	assert nested.exit_code == 1, nested.output
	assert nested.output.contains('pins V 98.98.98'), nested.output
	assert !nested.output.contains('PINNED'), nested.output
	// A separate repository below the pinned project is outside its scope.
	os.rm(os.join_path(subdir, '.vvmrc'))!
	os.mkdir(os.join_path(subdir, '.git'))!
	boundary := run_vvmrc_command(subdir, ['help'], envs)
	assert boundary.exit_code == 0, boundary.output
	assert !boundary.output.contains('PINNED'), boundary.output
	outside := run_vvmrc_command(root, ['help'], envs)
	assert outside.exit_code == 0, outside.output
	assert !outside.output.contains('PINNED'), outside.output
}

fn test_missing_project_pin_fails_closed_unless_explicitly_bypassed() {
	root := os.join_path(os.vtmp_dir(), 'vvmrc_missing_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	pin := os.join_path(root, '.vvmrc')
	os.write_file(pin, '98.98.98\n')!
	resolved_pin := os.real_path(pin)
	mut envs := os.environ()
	envs['V_SKIP_VVMRC'] = ''
	envs['VFLAGS'] = ''
	for args in [['fmt', '-verify', 'main.v'], ['help'], ['run', 'main.v']] {
		result := run_vvmrc_command(root, args, envs)
		assert result.exit_code == 1, result.output
		assert result.output.contains('pins V 98.98.98 (${resolved_pin})'), result.output
		assert result.output.contains('V_SKIP_VVMRC=1'), result.output
		assert !result.output.contains('continuing with'), result.output
	}
	envs['V_SKIP_VVMRC'] = '1'
	bypassed := run_vvmrc_command(root, ['help'], envs)
	assert bypassed.exit_code == 0, bypassed.output
	envs['V_SKIP_VVMRC'] = ''
	running := cmdexec.run(@VEXE, ['version'])
	assert running.exit_code == 0, running.output
	version_fields := running.output.trim_space().fields()
	assert version_fields.len == 3, running.output
	for same_version in [version_fields[1], 'v' + version_fields[1], version_fields[2], 'latest',
		'current'] {
		os.write_file(pin, same_version + '\n')!
		result := run_vvmrc_command(root, ['help'], envs)
		assert result.exit_code == 0, '${same_version}: ${result.output}'
	}
}
