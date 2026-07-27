// vtest build: amd64
import os

const routing_vexe = @VEXE
const routing_tests_dir = os.dir(@FILE)
const routing_v3_dir = os.dir(routing_tests_dir)
const routing_v3_src = os.join_path(routing_v3_dir, 'v3.v')
const routing_vlib_dir = os.dir(routing_v3_dir)

fn routing_path_present(path string) bool {
	return os.exists(path) || os.is_link(path)
}

fn routing_execute(executable string, args []string) os.Result {
	mut command := [os.quoted_path(executable)]
	for arg in args {
		command << os.quoted_path(arg)
	}
	return os.execute(command.join(' '))
}

fn routing_cleanup_file(path string) {
	if !routing_path_present(path) {
		return
	}
	assert !os.is_dir(path) || os.is_link(path), 'cleanup path `${path}` is an unexpected directory'
	os.rm(path) or { assert false, 'failed to remove `${path}`: ${err.msg()}' }
	assert !routing_path_present(path), 'cleanup left `${path}` behind'
}

fn routing_cleanup_tree(path string) {
	if !routing_path_present(path) {
		return
	}
	assert os.is_dir(path) && !os.is_link(path), 'cleanup root `${path}` is not a real directory'
	assert_closed_routing_inventory(path)
	os.rmdir_all(path) or { assert false, 'failed to remove `${path}`: ${err.msg()}' }
	assert !routing_path_present(path), 'cleanup left `${path}` behind'
}

fn build_amd64_routing_v3(v3_bin string) {
	assert !routing_path_present(v3_bin), 'compiler output `${v3_bin}` was stale'
	module_path := '${routing_vlib_dir}|@vlib|@vmodules'
	build := routing_execute(routing_vexe, ['-gc', 'none', '-path', module_path, '-o', v3_bin,
		routing_v3_src])
	assert build.exit_code == 0, build.output
	assert os.is_file(v3_bin) && !os.is_link(v3_bin), 'missing compiler `${v3_bin}`'
}

fn routing_create_workspace(root string) {
	assert !routing_path_present(root), 'workspace `${root}` was stale'
	os.mkdir(root) or { assert false, 'failed to create `${root}`: ${err.msg()}' }
	assert os.is_dir(root) && !os.is_link(root), 'workspace `${root}` is not a real directory'
}

fn routing_write_source(path string) {
	os.write_file(path, 'module main\n\nfn helper() {}\n\nfn main() {\n\thelper()\n}\n') or {
		panic(err)
	}
}

fn assert_closed_routing_inventory(root string) {
	for entry in os.ls(root) or { panic(err) } {
		full_path := os.join_path(root, entry)
		assert !entry.ends_with('.c'), 'unexpected C artifact `${full_path}`'
		assert !entry.ends_with('.v3cc'), 'unexpected C build artifact `${full_path}`'
		assert !entry.ends_with('.amd64-stage'), 'unexpected AMD64 stage `${full_path}`'
		if os.is_dir(full_path) && !os.is_link(full_path) {
			assert_closed_routing_inventory(full_path)
		}
	}
}

fn assert_no_c_fallback_or_stage(root string, output_file string, result os.Result) {
	assert !result.output.contains('C compilation failed:'), result.output
	for line in result.output.split('\n') {
		assert !line.starts_with('  > '), 'C command transcript survived:\n${result.output}'
	}
	assert !result.output.to_lower().contains('tcc'), result.output
	assert !result.output.contains('type checker found '), result.output
	assert !result.output.contains('V panic:'), result.output
	assert !routing_path_present(output_file + '.c')
	assert !routing_path_present(output_file + '.amd64-stage')
	assert_closed_routing_inventory(root)
}

fn assert_phase(result os.Result, phase string, detail string) {
	assert result.exit_code != 0
	assert result.output.contains('${phase}: ${detail}'), result.output
	for other in ['amd64 routing error', 'amd64 preflight error', 'amd64 SSA lowering error',
		'amd64 generation error', 'amd64 publication error'] {
		if other != phase {
			assert !result.output.contains('${other}:'), result.output
		}
	}
}

fn assert_amd64_failure(root string, output_file string, result os.Result, phase string, detail string) {
	assert_phase(result, phase, detail)
	assert !routing_path_present(output_file), 'failed command published `${output_file}`'
	assert_no_c_fallback_or_stage(root, output_file, result)
}

fn assert_runtime_global_upstream_standby(root string, output_file string, result os.Result) {
	assert result.exit_code != 0
	expected := 'amd64 SSA lowering error: amd64: global 0: type must be int_t, got 0'
	mut diagnostics := []string{}
	for line in result.output.split('\n') {
		if line.starts_with('amd64 ') && line.contains(' error:') {
			diagnostics << line
		}
	}
	assert diagnostics == [expected], result.output
	assert !routing_path_present(output_file), 'blocked command published `${output_file}`'
	assert_no_c_fallback_or_stage(root, output_file, result)
}

fn test_amd64_routing_is_object_only_and_fail_closed() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_amd64_routing_boot_${os.getpid()}')
	root := os.join_path(os.temp_dir(), 'v3_amd64_routing_object_only_${os.getpid()}')
	assert !routing_path_present(v3_bin), 'compiler output `${v3_bin}` was stale'
	assert !routing_path_present(root), 'workspace `${root}` was stale'
	defer {
		routing_cleanup_tree(root)
		routing_cleanup_file(v3_bin)
	}
	build_amd64_routing_v3(v3_bin)
	routing_create_workspace(root)
	source := os.join_path(root, 'program.v')
	routing_write_source(source)

	debug_output := os.join_path(root, 'debug.o')
	debug_result := routing_execute(v3_bin, ['-b', 'amd64', '-os', 'linux', '-o', debug_output,
		source])
	assert_runtime_global_upstream_standby(root, debug_output, debug_result)

	for target_os, suffix in {
		'linux':   '.o'
		'macos':   '.o'
		'windows': '.obj'
	} {
		profile_source := os.join_path(root, 'default_${target_os}.v')
		routing_write_source(profile_source)
		default_output := profile_source.all_before_last('.v') + suffix
		assert !routing_path_present(default_output)
		result := routing_execute(v3_bin, ['-b', 'amd64', '-os', target_os, profile_source])
		assert_runtime_global_upstream_standby(root, default_output, result)
	}

	unsupported_output := os.join_path(root, 'unsupported.o')
	unsupported := routing_execute(v3_bin, ['-b', 'amd64', '-os', 'freebsd', '-o', unsupported_output,
		source])
	assert_amd64_failure(root, unsupported_output, unsupported, 'amd64 preflight error',
		'target OS `freebsd` normalizes to unsupported `freebsd`')

	run_output := source.all_before_last('.v') + '.o'
	run_result := routing_execute(v3_bin, ['-b', 'amd64', 'run', source])
	assert_amd64_failure(root, run_output, run_result, 'amd64 preflight error',
		'run mode is unsupported; AMD64 emits relocatable objects only')

	test_output := os.join_path(root, 'test-command.o')
	test_result := routing_execute(v3_bin, ['-b', 'amd64', 'test', '-o', test_output, source])
	assert_amd64_failure(root, test_output, test_result, 'amd64 preflight error',
		'test mode is unsupported; AMD64 emits relocatable objects only')

	shared_output := os.join_path(root, 'shared.o')
	shared_result := routing_execute(v3_bin,
		['-b', 'amd64', '-shared', '-o', shared_output, source])
	assert_amd64_failure(root, shared_output, shared_result, 'amd64 preflight error',
		'shared output is unsupported; AMD64 emits relocatable objects only')

	test_source := os.join_path(root, 'selected_test.v')
	routing_write_source(test_source)
	test_file_output := os.join_path(root, 'test-file.o')
	test_file_result := routing_execute(v3_bin,
		['-b', 'amd64', '-o', test_file_output, test_source])
	assert_amd64_failure(root, test_file_output, test_file_result, 'amd64 preflight error',
		'test inputs are unsupported; AMD64 emits relocatable objects only')

	prod_output := os.join_path(root, 'prod.o')
	prod_result := routing_execute(v3_bin, ['-b', 'amd64', '-prod', '-os', 'linux', '-o', prod_output,
		source])
	assert_runtime_global_upstream_standby(root, prod_output, prod_result)

	autofree_output := os.join_path(root, 'autofree.o')
	autofree_result := routing_execute(v3_bin, ['-b', 'amd64', '-d', 'autofree', '-os', 'linux',
		'-o', autofree_output, source])
	assert_runtime_global_upstream_standby(root, autofree_output, autofree_result)
}
