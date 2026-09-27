module c

import strings
import v.pref

fn test_tinyc_windows_autostr_tls_resolves_fls_at_runtime() {
	preferences := pref.Preferences{}
	mut reflection_strings := map[string]int{}
	mut g := Gen{
		pref: &preferences
		anon_fn: unsafe { nil }
		reflection_strings: &reflection_strings
	}
	mut builder := strings.new_builder(1024)
	g.write_autostr_tls_global(mut builder, '', 'AutostrState', 'g_autostr_addr_state')
	windows_code := builder.str().all_before('#elif defined(__TINYC__)')
	assert windows_code.contains('GetProcAddress(kernel32, "FlsAlloc")')
	assert windows_code.contains('GetProcAddress(kernel32, "FlsGetValue")')
	assert windows_code.contains('GetProcAddress(kernel32, "FlsSetValue")')
	assert windows_code.contains('fls_alloc(g_autostr_addr_state_tls_free)')
	assert windows_code.contains('g_autostr_addr_state_fls_get(g_autostr_addr_state_tls_key)')
	assert windows_code.contains('g_autostr_addr_state_fls_set(g_autostr_addr_state_tls_key, p)')
	assert !windows_code.contains('FlsAlloc(g_autostr_addr_state_tls_free)')
	assert !windows_code.contains('FlsGetValue(g_autostr_addr_state_tls_key)')
	assert !windows_code.contains('FlsSetValue(g_autostr_addr_state_tls_key, p)')
}
