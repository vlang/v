import dl
import dl.loader
import os

const loader_env_var = 'V_TESTCOV_DL_LOADER_PATH'

// A loader for a library that cannot exist on any platform: the directory in
// the path is invented, so no search path can satisfy it.
const missing_lib_path = 'no-such-directory-zzz/no-such-library-zzz'

fn test_loader_error_constants() {
	assert loader.dl_no_path_issue_err.msg() == 'no paths to dynamic library'
	assert loader.dl_open_issue_err.msg() == 'could not open dynamic library'
	assert loader.dl_sym_issue_err.msg() == 'could not get optional symbol from dynamic library'
	assert loader.dl_close_issue_err.msg() == 'could not close dynamic library'
	assert loader.dl_register_issue_err.msg() == 'could not register dynamic library loader'

	assert loader.dl_no_path_issue_code == 1
	assert loader.dl_open_issue_code == 1
	assert loader.dl_sym_issue_code == 2
	assert loader.dl_close_issue_code == 3
	assert loader.dl_register_issue_code == 4
	assert loader.dl_no_path_issue_err.code() == loader.dl_no_path_issue_code
	assert loader.dl_open_issue_err.code() == loader.dl_open_issue_code
	assert loader.dl_sym_issue_err.code() == loader.dl_sym_issue_code
	assert loader.dl_close_issue_err.code() == loader.dl_close_issue_code
	assert loader.dl_register_issue_err.code() == loader.dl_register_issue_code
	// NOTE: the "no paths" and "could not open" conditions share code 1.
}

fn test_a_loader_without_any_path_is_refused() {
	if _ := loader.get_or_create_dynamic_lib_loader(key: 'vcov.loader.nopaths') {
		assert false, 'a loader with no paths was created'
	} else {
		assert err.msg() == loader.dl_no_path_issue_err.msg()
		assert err.code() == loader.dl_no_path_issue_code
	}
	if _ := loader.get_or_create_dynamic_lib_loader(key: 'vcov.loader.nopaths', paths: []) {
		assert false, 'a loader with an empty path list was created'
	} else {
		assert err.msg() == loader.dl_no_path_issue_err.msg()
	}
	assert 'vcov.loader.nopaths' !in loader.registered_dl_loader_keys()
}

fn test_loader_keeps_its_key_paths_and_default_flags() {
	mut l := loader.get_or_create_dynamic_lib_loader(
		key:   'vcov.loader.basic'
		paths: [
			'/one',
			'/two',
		]
	)!
	defer {
		l.unregister()
	}
	assert l.key == 'vcov.loader.basic'
	assert l.paths == ['/one', '/two']
	// The config default is the lazy resolve flag of the host platform.
	assert l.flags == dl.rtld_lazy
}

fn test_loader_takes_the_flags_it_is_given() {
	mut l := loader.get_or_create_dynamic_lib_loader(
		key:   'vcov.loader.flags'
		paths: [
			'/one',
		]
		flags: dl.rtld_now
	)!
	defer {
		l.unregister()
	}
	assert l.flags == dl.rtld_now
}

fn test_env_path_is_split_on_the_platform_delimiter_and_prepended() {
	os.setenv(loader_env_var, '/a${os.path_delimiter}/b${os.path_delimiter}/c', true)
	mut l := loader.get_or_create_dynamic_lib_loader(
		key:      'vcov.loader.envpath'
		env_path: loader_env_var
		paths:    ['/d']
	)!
	defer {
		l.unregister()
	}
	assert l.paths == ['/a', '/b', '/c', '/d']
	os.unsetenv(loader_env_var)
}

fn test_an_unset_env_path_contributes_no_paths() {
	os.unsetenv(loader_env_var)
	mut l := loader.get_or_create_dynamic_lib_loader(
		key:      'vcov.loader.noenv'
		env_path: loader_env_var
		paths:    ['/only']
	)!
	defer {
		l.unregister()
	}
	assert l.paths == ['/only']
}

fn test_open_reports_an_error_when_no_path_can_be_loaded() {
	mut l := loader.get_or_create_dynamic_lib_loader(
		key:   'vcov.loader.openfail'
		paths: [
			missing_lib_path,
		]
	)!
	defer {
		l.unregister()
	}
	if _ := l.open() {
		assert false, 'open loaded ${missing_lib_path}'
	} else {
		assert err.msg() == loader.dl_open_issue_err.msg()
		assert err.code() == loader.dl_open_issue_code
	}
}

fn test_close_reports_an_error_when_nothing_is_open() {
	mut l := loader.get_or_create_dynamic_lib_loader(
		key:   'vcov.loader.closefail'
		paths: [
			missing_lib_path,
		]
	)!
	defer {
		l.unregister()
	}
	// The open attempt on purpose: it must fail and leave the handle empty.
	l.open() or {}
	if _ := l.close() {
		assert false, 'close succeeded with no open handle'
	} else {
		assert err.msg() == loader.dl_close_issue_err.msg()
		assert err.code() == loader.dl_close_issue_code
	}
}

fn test_get_sym_propagates_the_open_error() {
	mut l := loader.get_or_create_dynamic_lib_loader(
		key:   'vcov.loader.symfail'
		paths: [
			missing_lib_path,
		]
	)!
	defer {
		l.unregister()
	}
	if _ := l.get_sym('anything') {
		assert false, 'get_sym resolved a symbol from an unloadable library'
	} else {
		assert err.msg() == loader.dl_open_issue_err.msg()
		assert err.code() == loader.dl_open_issue_code
	}
}

fn test_get_or_create_returns_the_registered_loader() {
	mut first := loader.get_or_create_dynamic_lib_loader(
		key:   'vcov.loader.dup'
		paths: [
			'/one',
			'/two',
		]
	)!
	mut second := loader.get_or_create_dynamic_lib_loader(
		key:   'vcov.loader.dup'
		paths: [
			'/other',
		]
	)!
	assert first == second
	// The second call finds the registration and never sees its own paths.
	assert second.paths == ['/one', '/two']
	second.unregister()
	assert 'vcov.loader.dup' !in loader.registered_dl_loader_keys()
}

fn test_a_loader_key_can_be_reused_after_unregister() {
	key := 'vcov.loader.reuse'
	mut first := loader.get_or_create_dynamic_lib_loader(
		key:   key
		paths: [
			'/one',
		]
	)!
	first.unregister()
	mut second := loader.get_or_create_dynamic_lib_loader(
		key:   key
		paths: [
			'/two',
		]
	)!
	defer {
		second.unregister()
	}
	assert first != second
	assert second.paths == ['/two']
	assert key in loader.registered_dl_loader_keys()
}

fn test_registered_loader_keys_only_reports_what_is_registered() {
	key := 'vcov.loader.keys'
	assert key !in loader.registered_dl_loader_keys()
	mut l := loader.get_or_create_dynamic_lib_loader(
		key:   key
		paths: [
			'/one',
		]
	)!
	assert key in loader.registered_dl_loader_keys()
	l.unregister()
	assert key !in loader.registered_dl_loader_keys()
}
