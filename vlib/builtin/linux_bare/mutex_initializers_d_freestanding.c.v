module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	// GNU's public static initializer macros carry their mutex kind outside
	// this runtime's lock/owner words. Publish it once before acquiring the lock.
	@[export: 'v_native_mutex_initializer'; markused]
	fn bare_native_mutex_initializer(attributes uint, kind i32, recursive i32, errorcheck i32) {
		if kind == recursive || kind == errorcheck {
			bare_cas_u32(unsafe { &u32(attributes) }, 0, u32(kind))
		}
	}
}
