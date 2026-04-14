module native

import v.pref

fn new_test_gen(p &pref.Preferences) &Gen {
	return macho_test_new_gen(p, 'test.bin')
}

fn test_comptime_ident_uses_shared_builtin_tables() {
	mut g := new_test_gen(&pref.Preferences{
		arch:    .amd64
		m64:     true
		os:      .macos
		backend: .native
	})
	assert g.comptime_ident('x64', false)
	assert !g.comptime_ident('x32', false)
	assert !g.comptime_ident('aarch64', false)
	assert !g.comptime_ident('wasm32_emscripten', false)
}

fn test_comptime_ident_respects_optional_custom_defines() {
	mut g := new_test_gen(&pref.Preferences{
		compile_defines:     ['custom_define']
		compile_defines_all: ['custom_define', 'no_backtrace']
	})
	assert g.comptime_ident('custom_define', true)
	assert !g.comptime_ident('missing_define', true)
}
