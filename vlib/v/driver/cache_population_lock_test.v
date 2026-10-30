module driver

import os
import time
import v.modulecache

const population_helper_mode_env = 'V3_TEST_POPULATION_LOCK_HELPER'
const population_helper_dir_env = 'V3_TEST_POPULATION_LOCK_DIR'

fn testsuite_begin() {
	if os.getenv(population_helper_mode_env) != '' {
		run_population_lock_helper()
	}
}

// run_population_lock_helper is the other build: `hold` takes the lock and keeps
// it until its release file appears, then exits without releasing it; `try`
// reports in its exit code whether it got the lock.
fn run_population_lock_helper() {
	dir := os.getenv(population_helper_dir_env)
	match os.getenv(population_helper_mode_env) {
		'hold' {
			if acquire_v3_cache_population_lock(dir) != .acquired {
				exit(2)
			}
			println('locked')
			os.flush()
			for _ in 0 .. 3000 {
				if os.exists(os.join_path(dir, 'release')) {
					break
				}
				time.sleep(10 * time.millisecond)
			}
			exit(0)
		}
		'try' {
			exit(match acquire_v3_cache_population_lock(dir) {
				.acquired { 0 }
				.held_elsewhere { 1 }
				.unusable { 4 }
			})
		}
		'handoff-parent' {
			if acquire_v3_cache_population_lock(dir) != .acquired {
				exit(31)
			}
			marker := os.join_path(dir, 'fallback-marker')
			os.setenv(macos_v3_fallback_file_env, marker, true)
			if !stage_macos_v3_compiler_error_fallback(marker, 'child handoff test')
				|| !os.is_file(marker) {
				exit(32)
			}
			os.setenv(population_helper_mode_env, 'handoff-child', true)
			exit_with_child_build(os.executable(), []string{})
		}
		'handoff-child' {
			if os.exists(os.join_path(dir, 'fallback-marker')) {
				exit(33)
			}
			if acquire_v3_cache_population_lock(dir) != .acquired {
				exit(34)
			}
			os.write_file(os.join_path(dir, 'child-executed'), '23') or { exit(35) }
			release_v3_cache_population_lock()
			exit(23)
		}
		else {
			exit(3)
		}
	}
}

fn new_population_lock_helper(dir string, mode string) &os.Process {
	mut p := os.new_process(os.executable())
	mut env := os.environ()
	env[population_helper_mode_env] = mode
	env[population_helper_dir_env] = dir
	p.set_environment(env)
	p.set_redirect_stdio()
	return p
}

fn other_build_gets_population_lock(dir string) bool {
	mut p := new_population_lock_helper(dir, 'try')
	p.run()
	p.wait()
	code := p.code
	p.close()
	assert code in [0, 1], 'the helper could not use the lock: exit ${code}'
	return code == 0
}

fn population_test_state(root string, cold_modules []string, module_sources map[string][]string) V3ModuleCacheState {
	old_v3cache := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	manager := modulecache.new_manager(@VEXEROOT, 'population lock test', true, '', '')
	if value := old_v3cache {
		os.setenv('V3CACHE', value, true)
	} else {
		os.unsetenv('V3CACHE')
	}
	mut state := V3ModuleCacheState{
		manager:        manager
		module_sources: module_sources
	}
	for name in cold_modules {
		state.parsed_from_source[name] = true
	}
	return state
}

fn test_large_cache_population_is_bounded() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_population_gate_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		release_v3_cache_population_lock()
		os.rmdir_all(root) or {}
	}
	small := os.join_path(root, 'small.v')
	os.write_file(small, 'module small\n')!
	sources := {
		'small': [small]
		'again': [small]
	}
	limit := large_cold_cache_population_node_limit
	compiler_limit := compiler_tree_large_cold_cache_node_limit
	large := scoped_large_cold_cache_node_limit
	// A graph beyond the scoped limit may populate the cache within these bounds.
	warm := population_test_state(root, [], sources)
	assert v3_large_cache_population(false, warm, large) == .populate
	// Existing cache hits are not limited by cold-population bounds.
	assert v3_large_cache_population(false, warm, limit) == .populate
	assert v3_large_cache_population(true, warm, compiler_limit) == .populate
	assert v3_large_cache_population(false, warm, limit - 1) == .populate
	assert v3_large_cache_population(true, warm, compiler_limit - 1) == .populate
	// Nothing to publish: no lock is needed.
	assert !os.exists(os.join_path(warm.manager.dir, v3_cache_population_lock_name))

	cold := population_test_state(root, ['small'], sources)
	assert v3_large_cache_population(false, cold, large) == .populate
	assert v3_large_cache_population(false, cold, limit - 1) == .populate
	// Cold population above the bound keeps the bypass.
	assert v3_large_cache_population(false, cold, limit) == .bypass
	assert v3_large_cache_population(true, cold, compiler_limit) == .bypass
	assert os.exists(os.join_path(cold.manager.dir, v3_cache_population_lock_name))
	// Asking again keeps the lock this build already holds. (On POSIX a process can
	// take its own record lock again anyway; Windows refuses a second handle.)
	assert v3_large_cache_population(false, cold, limit - 1) == .populate

	// The sources of every imported module count, cached or not, each file once.
	huge := os.join_path(root, 'huge.v')
	os.write_file(huge, 'x')!
	mut huge_sources := sources.clone()
	huge_sources['huge'] = [huge]
	too_large := population_test_state(root, ['small'], huge_sources)
	os.truncate(huge, u64(large_cold_cache_population_source_limit + 1 - os.file_size(small)))!
	assert v3_cache_module_source_bytes(too_large) == large_cold_cache_population_source_limit + 1
	assert v3_large_cache_population(false, too_large, limit - 1) == .bypass
	// The limit itself is admitted.
	os.truncate(huge, u64(large_cold_cache_population_source_limit - os.file_size(small)))!
	assert v3_cache_module_source_bytes(too_large) == large_cold_cache_population_source_limit
	assert v3_large_cache_population(false, too_large, limit - 1) == .populate
	// Compiler-module tests retain lower node and source bounds.
	os.truncate(huge, u64(compiler_tree_large_cold_cache_source_limit + 1 - os.file_size(small)))!
	assert v3_large_cache_population(true, too_large, limit - 1) == .bypass
	os.truncate(huge, u64(compiler_tree_large_cold_cache_source_limit - os.file_size(small)))!
	assert v3_large_cache_population(true, too_large, compiler_limit - 1) == .populate
	// The builtin bundle counts too.
	bundled := V3ModuleCacheState{
		manager:        too_large.manager
		module_sources: sources
		bundle_sources: [small, huge]
	}
	assert v3_cache_module_source_bytes(bundled) == os.file_size(small) + os.file_size(huge)
}

fn test_large_cache_population_keeps_the_bypass_without_a_usable_lock() ! {
	$if windows {
		// Windows has no portable way here to make a file unwritable to its owner.
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_population_readonly_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		release_v3_cache_population_lock()
		os.chmod(root, 0o755) or {}
		os.rmdir_all(root) or {}
	}
	state := population_test_state(root, ['types'], map[string][]string{})
	os.mkdir_all(state.manager.dir)!
	os.chmod(state.manager.dir, 0o555)!
	defer {
		os.chmod(state.manager.dir, 0o755) or {}
	}
	if os.is_writable(state.manager.dir) {
		// Running as root: permissions do not stop it.
		return
	}
	assert acquire_v3_cache_population_lock(state.manager.dir) == .unusable
	assert v3_large_cache_population(true, state, scoped_large_cold_cache_node_limit) == .bypass
}

fn test_one_build_at_a_time_holds_the_population_lock() {
	root := os.join_path(os.vtmp_dir(), 'v3_population_lock_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		release_v3_cache_population_lock()
		os.rmdir_all(root) or {}
	}
	state := population_test_state(root, ['types'], map[string][]string{})
	dir := state.manager.dir
	assert acquire_v3_cache_population_lock(dir) == .acquired
	assert !other_build_gets_population_lock(dir)
	// Releasing it lets the build that this one waits for take it.
	release_v3_cache_population_lock()
	assert other_build_gets_population_lock(dir)

	mut holder := new_population_lock_helper(dir, 'hold')
	holder.run()
	mut output := ''
	for _ in 0 .. 500 {
		if holder.is_pending(.stdout) {
			output += holder.stdout_read()
		}
		if output.contains('locked') || !holder.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert output.contains('locked')
	assert acquire_v3_cache_population_lock(dir) == .held_elsewhere
	assert v3_large_cache_population(true, state, scoped_large_cold_cache_node_limit) == .contended
	// The process that checks the instances of a build's generics follows its
	// build, which holds the lock.
	os.setenv(instance_check_child_env, '1', true)
	child_decision := v3_large_cache_population(true, state, scoped_large_cold_cache_node_limit)
	os.unsetenv(instance_check_child_env)
	assert child_decision == .populate
	// The holder exits without releasing the lock: the system releases it.
	os.write_file(os.join_path(dir, 'release'), '')!
	holder.wait()
	holder.close()
	assert acquire_v3_cache_population_lock(dir) == .acquired
}

fn test_child_build_releases_the_population_lock_and_fallback_marker() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_population_child_handoff_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut parent := new_population_lock_helper(root, 'handoff-parent')
	parent.run()
	parent.wait()
	code := parent.code
	output := parent.stdout_read() + parent.stderr_read()
	parent.close()
	assert code == 23, 'the child build status was not forwarded: ${code}: ${output}'
	assert os.read_file(os.join_path(root, 'child-executed'))! == '23'
}
