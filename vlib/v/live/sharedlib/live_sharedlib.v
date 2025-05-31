module sharedlib

import v.live as _

@[export: 'set_live_reload_pointer']
@[markused]
pub fn set_live_reload_pointer(p voidptr) {
	// NOTE: the `g_live_reload_info` global on windows, in the DLL, has a different address itself,
	// compared to the g_live_reload_info in the main executable.
	//
	// The code here, ensures that *its value* will be the same,
	// since the executable, will make sure to load the DLL, and then call set_live_reload_pointer()
	// after binding it, in its generaged `v_bind_live_symbols`, with the value of its own `g_live_reload_info` global.
	//
	// This is not necessary on macos and linux, but it is best to have the same code across systems anyway.	
	// eprintln('>>>>> before &g_live_reload_info: ${voidptr(&g_live_reload_info)} | g_live_reload_info: ${voidptr(g_live_reload_info)}')
	g_live_reload_info = p
	// eprintln('>>>>>  after &g_live_reload_info: ${voidptr(&g_live_reload_info)} | g_live_reload_info: ${voidptr(g_live_reload_info)}')
}
