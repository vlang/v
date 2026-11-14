module testing

import os
import rand

struct CacheSessionTestEnvironment {
	root     string
	previous map[string]string
}

fn new_cache_session_test_environment() !CacheSessionTestEnvironment {
	mut previous := map[string]string{}
	for name in ['VTMP', 'V3CACHE', 'V3_TEST_ISOLATE_CACHE'] {
		if value := os.getenv_opt(name) {
			previous[name] = value
		}
	}
	root := os.join_path(os.vtmp_dir(), 'cache session ${rand.ulid()}')
	os.mkdir_all(root)!
	os.setenv('VTMP', root, true)
	os.unsetenv('V3CACHE')
	os.unsetenv('V3_TEST_ISOLATE_CACHE')
	return CacheSessionTestEnvironment{root, previous}
}

fn (environment CacheSessionTestEnvironment) restore() {
	for name in ['VTMP', 'V3CACHE', 'V3_TEST_ISOLATE_CACHE'] {
		if name in environment.previous {
			os.setenv(name, environment.previous[name], true)
		} else {
			os.unsetenv(name)
		}
	}
	os.rmdir_all(environment.root) or { panic(err) }
}

fn test_vtmp_sessions_share_the_incoming_cache_root() {
	environment := new_cache_session_test_environment()!
	defer {
		environment.restore()
	}
	shared_cache := os.join_path(environment.root, 'vtest_module_cache')
	marker := os.join_path(environment.root, 'shared-cache-marker')
	os.write_file(marker, 'cache survives session cleanup')!
	first := setup_new_vtmp_folder('first')
	assert first == os.join_path(environment.root, 'tsession_first')
	assert os.getenv('V3CACHE') == shared_cache
	assert os.getenv('VTMP') == first
	assert os.is_file(os.join_path(first, '.v.mod.stop'))
	nested := setup_new_vtmp_folder('nested')
	assert nested == os.join_path(first, 'tsession_nested')
	assert os.getenv('V3CACHE') == shared_cache
	assert os.getenv('VTMP') == nested
	assert os.is_file(os.join_path(nested, '.v.mod.stop'))
	os.rmdir_all(first)!
	assert os.read_file(marker)! == 'cache survives session cleanup'
	// A subsequent top-level session reuses the same cache after the first was removed.
	os.setenv('VTMP', environment.root, true)
	second := setup_new_vtmp_folder('second')
	assert second == os.join_path(environment.root, 'tsession_second')
	assert os.getenv('V3CACHE') == shared_cache
	assert os.getenv('VTMP') == second
	assert os.is_file(os.join_path(second, '.v.mod.stop'))
}

fn test_vtmp_sessions_preserve_an_explicit_cache_even_when_isolation_is_requested() {
	environment := new_cache_session_test_environment()!
	defer {
		environment.restore()
	}
	explicit_cache := os.join_path(environment.root, 'explicit cache')
	for index, isolate in ['', '1'] {
		os.setenv('VTMP', environment.root, true)
		os.setenv('V3CACHE', explicit_cache, true)
		os.setenv('V3_TEST_ISOLATE_CACHE', isolate, true)
		session := setup_new_vtmp_folder('explicit_${index}')
		assert os.getenv('V3CACHE') == explicit_cache
		assert os.getenv('VTMP') == session
		assert session == os.join_path(environment.root, 'tsession_explicit_${index}')
		assert os.is_file(os.join_path(session, '.v.mod.stop'))
	}
}

fn test_vtmp_sessions_leave_an_unset_cache_unset_when_isolation_is_requested() {
	environment := new_cache_session_test_environment()!
	defer {
		environment.restore()
	}
	os.setenv('V3_TEST_ISOLATE_CACHE', '1', true)
	session := setup_new_vtmp_folder('isolated')
	assert 'V3CACHE' !in os.environ()
	assert session == os.join_path(environment.root, 'tsession_isolated')
	assert os.getenv('VTMP') == session
	assert os.is_file(os.join_path(session, '.v.mod.stop'))
}

fn test_vtmp_sessions_keep_the_shared_test_cache_apart_from_the_default_cache() {
	environment := new_cache_session_test_environment()!
	defer {
		environment.restore()
	}
	setup_new_vtmp_folder('apart')
	assert os.getenv('V3CACHE') != environment.root
	assert os.getenv('V3CACHE') == os.join_path(environment.root, 'vtest_module_cache')
}

fn test_cache_tests_and_tests_choosing_their_cache_keep_out_of_the_shared_cache() {
	root := os.join_path(os.vtmp_dir(), 'own cache ${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	plain := os.join_path(root, 'plain_test.v')
	os.write_file(plain, 'fn test_plain() {\n\tassert true\n}\n')!
	assert !test_uses_own_module_cache(plain, get_test_details(plain))
	nocache := os.join_path(root, 'runs_v_test.v')
	os.write_file(nocache, "fn test_run() {\n\targs := ['-nocache']\n\tassert args.len == 1\n}\n")!
	assert test_uses_own_module_cache(nocache, get_test_details(nocache))
	v3cache := os.join_path(root, 'sets_cache_dir_test.v')
	os.write_file(v3cache, "fn test_env() {\n\tassert 'V3CACHE'.len > 0\n}\n")!
	assert test_uses_own_module_cache(v3cache, get_test_details(v3cache))
	none_details := TestDetails{}
	for name in ['/r/vlib/v/modulecache/file_signature_test.v',
		'/r/vlib/v/types/ierror_type_cache_test.v', '/r/vlib/v/driver/unchanged_build_object_test.v',
		'/r/vlib/v/driver/program_executable_reuse_test.v'] {
		assert test_uses_own_module_cache(name, none_details), name
	}
	for name in ['/r/vlib/v/types/alias_cycle_id_test.v', '/r/vlib/v/gen/c/cached/plain_test.v'] {
		assert !test_uses_own_module_cache(name, none_details), name
	}
}

fn test_type_checker_tests_build_without_the_module_cache() {
	assert test_builds_without_cache('/r/vlib/v/types/alias_cycle_id_test.v')
	assert !test_builds_without_cache('/r/vlib/v/transform/match_clone_test.v')
	assert !test_builds_without_cache('/r/vlib/v/gen/c/map_test.v')
	assert !test_builds_without_cache('/r/vlib/v/types_extra/alias_test.v')
}
