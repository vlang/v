import dl

fn test_shared_library_extension_is_platform_specific() {
	$if windows {
		assert dl.get_shared_library_extension() == '.dll'
	} $else $if macos {
		assert dl.get_shared_library_extension() == '.dylib'
	} $else {
		assert dl.get_shared_library_extension() == '.so'
	}
	assert dl.dl_ext == dl.get_shared_library_extension()
	assert dl.dl_ext.len > 1
	assert dl.dl_ext[0] == `.`
}

fn test_get_libname_appends_the_platform_extension() {
	assert dl.get_libname('foo') == 'foo${dl.dl_ext}'
	assert dl.get_libname('foo.bar') == 'foo.bar${dl.dl_ext}'
	assert dl.get_libname('') == dl.dl_ext
	assert dl.get_libname(dl.dl_ext) == '${dl.dl_ext}${dl.dl_ext}'
	assert dl.get_libname('a').len == 1 + dl.dl_ext.len
}

fn test_version_constant() {
	assert dl.version == 1
}

// A path whose directory cannot exist, so no platform can resolve it. The
// message itself is platform specific, so only its presence is asserted.
fn test_open_opt_reports_an_error_for_an_unloadable_library() {
	missing := 'no-such-directory-zzz/no-such-library-zzz'
	if _ := dl.open_opt(missing, dl.rtld_now) {
		assert false, 'open_opt loaded ${missing}'
	} else {
		assert err.msg().len > 0
		assert err.code() == 0
	}
	if _ := dl.open_opt(missing, dl.rtld_lazy) {
		assert false, 'open_opt loaded ${missing}'
	} else {
		assert err.msg().len > 0
	}
}

// A null handle plus a symbol that cannot exist. Windows resolves it through
// GetProcAddress(NULL, ...) and every POSIX platform through dlsym(NULL, ...),
// which searches the global scope; neither can return it.
fn test_sym_opt_reports_an_error_for_an_unknown_symbol() {
	unknown := 'v_no_such_symbol_zzz_12345'
	if _ := dl.sym_opt(voidptr(0), unknown) {
		assert false, 'sym_opt resolved ${unknown}'
	} else {
		assert err.msg().len > 0
	}
}

fn test_rtld_flag_constants_are_usable_as_integers() {
	for flags in [dl.rtld_now, dl.rtld_lazy, dl.rtld_global, dl.rtld_local, dl.rtld_nodelete,
		dl.rtld_noload] {
		// The flag values themselves are platform defined; every accepted
		// open_opt call has to take them without panicking.
		if _ := dl.open_opt('no-such-directory-zzz/no-such-library-zzz', flags) {
			assert false, 'open_opt loaded a library from a missing directory'
		} else {
			assert err.msg().len > 0
		}
	}
}
