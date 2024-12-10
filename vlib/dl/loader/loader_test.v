import dl.loader
import dl

fn test_dl_loader() ! {
	$if linux {
		run_test_invalid_lib_linux()!
		return
	}
	$if windows {
		run_test_invalid_lib_windows()!
		run_test_valid_lib_windows()!
		run_test_invalid_sym_windows()!
		run_test_valid_sym_windows()!
		return
	} $else {
		eprint('currently not implemented on this platform')
	}
}

fn get_or_create_loader(name string, paths []string) !&loader.DynamicLibLoader {
	return loader.get_or_create_dynamic_lib_loader(
		key:   name
		paths: paths
		flags: dl.rtld_now
	)
}

fn run_test_invalid_lib_linux() ! {
	// ensure a not-existing dl won't be loaded
	mut dl_loader := get_or_create_loader(@MOD + '.' + @FN + '.' + 'lib', [
		'not-existing-dynamic-link-library',
	])!
	defer {
		dl_loader.unregister()
	}
	h := dl_loader.open() or { unsafe { nil } }
	assert isnil(h)
}

fn run_test_invalid_lib_windows() ! {
	// ensure a not-existing dl won't be loaded
	mut dl_loader := get_or_create_loader(@MOD + '.' + @FN + '.' + 'lib', [
		'not-existing-dynamic-link-library',
	])!
	defer {
		dl_loader.unregister()
	}
	h := dl_loader.open() or { unsafe { nil } }
	assert isnil(h)
}

fn run_test_valid_lib_windows() ! {
	mut dl_loader := get_or_create_loader(@MOD + '.' + @FN + '.' + 'lib', [
		'not-existing-dynamic-link-library',
		'shell32',
	])!
	defer {
		dl_loader.unregister()
	}
	h := dl_loader.open() or { unsafe { nil } }
	assert !isnil(h)
}

fn run_test_invalid_sym_windows() ! {
	mut dl_loader := get_or_create_loader(@MOD + '.' + @FN + '.' + 'lib', ['shell32'])!
	defer {
		dl_loader.unregister()
	}
	proc := dl_loader.get_sym('CommandLineToArgvW2') or { unsafe { nil } }
	assert isnil(proc)
}

fn run_test_valid_sym_windows() ! {
	mut dl_loader := get_or_create_loader(@MOD + '.' + @FN + '.' + 'lib', [
		'not-existing-dynamic-link-library',
		'shell32',
	])!
	defer {
		dl_loader.unregister()
	}
	proc := dl_loader.get_sym('CommandLineToArgvW') or { unsafe { nil } }
	assert !isnil(proc)
}
