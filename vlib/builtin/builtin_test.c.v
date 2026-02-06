fn test_g_main_argc() {
	assert g_main_argc > 0
}

fn test_g_main_argv() {
	assert g_main_argv != 0
	mut first_arg := ''
	$if windows {
		first_arg = unsafe { string_from_wide(&&u16(g_main_argv)[0]) }
	} $else {
		first_arg = unsafe { cstring_to_vstring(&&char(g_main_argv)[0]) }
	}
	assert first_arg.contains('builtin_test')
}

@[if windows]
fn test_bool_size() {
	println(@LOCATION)
	assert sizeof(C.BOOL) == 4
}

fn test_v_getpid__and__v_gettid() {
	p := v_getpid()
	dump(p)
	t := v_gettid()
	dump(t)
}
