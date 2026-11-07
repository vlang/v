module driver

import os
import v.cmdexec
import v.pref

fn native_environment_keys(compiler string) []string {
	mut stats := CObjectCacheStats{}
	_, identity := c_object_compiler_identity(compiler, mut stats)
	flags := CObjectFlagPlan{}
	target := pref.host_target()
	return [
		identity,
		c_object_manifest_path('/cache', '/probe.o', compiler, [], target, mut stats),
		c_link_plan_path('/cache', [], &flags, false, false, '', [], target, compiler, false, mut stats),
		c_object_cache_name('/probe.o', compiler, [], [], target, false, mut stats),
		v3_cached_object_compile_signature(V3CachedObjectCompiler{}, 'c11', '', '', '', [], false, ''),
	]
}

fn test_native_cache_keys_cover_header_and_toolchain_environment() {
	compiler := os.find_abs_path_of_executable('clang') or { return }
	for name in ['CPATH', 'C_INCLUDE_PATH', 'CPLUS_INCLUDE_PATH', 'OBJC_INCLUDE_PATH', 'INCLUDE',
		'CL', '_CL_', 'WindowsSdkDir', 'WindowsSDKVersion', 'VCToolsInstallDir', 'UCRTVersion'] {
		saved := os.getenv_opt(name)
		os.setenv(name, '/first-sdk', true)
		first := native_environment_keys(compiler)
		assert native_environment_keys(compiler) == first
		os.setenv(name, '/second-sdk', true)
		second := native_environment_keys(compiler)
		for index, key in first {
			assert second[index] != key, '${name}, cache key ${index}'
		}
		$if !windows {
			// Windows removes environment variables when their value is empty.
			os.setenv(name, '', true)
			empty := native_environment_keys(compiler)
			os.unsetenv(name)
			unset := native_environment_keys(compiler)
			assert empty != unset, name
		}
		if value := saved {
			os.setenv(name, value, true)
		} else {
			os.unsetenv(name)
		}
	}
}

fn test_flag_object_rebuilds_when_header_search_environment_changes() ! {
	$if windows {
		return
	}
	compiler := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'c_object_environment_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	app := os.join_path(root, 'app')
	probe := os.join_path(app, 'probe')
	os.mkdir_all(probe)!
	os.write_file(os.join_path(app, 'v.mod'), "Module { name: 'app' }\n")!
	os.write_file(os.join_path(app, 'main.v'), 'import probe\nfn main() { println(probe.abi()) }\n')!
	os.write_file(os.join_path(probe, 'probe.c.v'), 'module probe\n#flag @VMODROOT/probe/abi.o\nfn C.probe_abi() int\npub fn abi() int { return C.probe_abi() }\n')!
	os.write_file(os.join_path(probe, 'abi.c'), '#include <probe_config.h>\nint probe_abi(void) { return PROBE_ABI; }\n')!
	for value in [1, 2] {
		sdk := os.join_path(root, 'sdk${value}')
		os.mkdir_all(sdk)!
		os.write_file(os.join_path(sdk, 'probe_config.h'), '#define PROBE_ABI ${value}\n')!
	}
	for name in ['CPATH', 'C_INCLUDE_PATH'] {
		mut environment := os.environ()
		environment['VFLAGS'] = ''
		environment['CPATH'] = ''
		environment['C_INCLUDE_PATH'] = ''
		environment['V_MACOS_V3_NO_FALLBACK'] = '1'
		environment['VTMP'] = os.join_path(root, 'tmp_${name}')
		environment['V3CACHE'] = os.join_path(root, 'cache_${name}')
		program := os.join_path(root, 'program')
		for value in [1, 1, 2, 2, 1] {
			environment[name] = os.join_path(root, 'sdk${value}')
			result := cmdexec.run_in_merged_with_env(@VEXE, ['-cc', compiler, '-o', program, app], @VEXEROOT, environment)
			assert result.exit_code == 0, result.output
			run := cmdexec.run(program, [])
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == value.str(), '${name}: ${run.output}'
		}
	}
}
