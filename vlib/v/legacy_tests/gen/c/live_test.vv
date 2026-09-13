module c

import v.pref

fn test_live_reload_forwarded_define_vopts_quotes_simple_sokol_define() {
	vopts := live_reload_forwarded_define_vopts(pref.OS.linux, ['sokol_d3d11'], {
		'sokol_d3d11': 'true'
	})
	assert vopts == "-d 'sokol_d3d11'"
}

fn test_live_reload_forwarded_define_vopts_quotes_posix_shell_metachar_values() {
	dollar := '$'
	vopts := live_reload_forwarded_define_vopts(pref.OS.linux,
		['sokol_semicolon', 'sokol_subshell'], {
		'sokol_semicolon': 'a;b'
		'sokol_subshell':  '${dollar}(id)'
	})
	assert vopts.contains("-d 'sokol_semicolon=a;b'")
	assert vopts.contains("-d 'sokol_subshell=${dollar}(id)'")
}

fn test_live_reload_forwarded_define_vopts_quotes_windows_shell_metachar_values_for_c_string() {
	vopts := live_reload_forwarded_define_vopts(pref.OS.windows, [
		'sokol_ampersand',
		'sokol_pipe',
		'sokol_percent',
	], {
		'sokol_ampersand': 'a&b'
		'sokol_pipe':      'a|b'
		'sokol_percent':   '%TEMP%'
	})
	assert vopts.contains(r'-d \"sokol_ampersand=a&b\"')
	assert vopts.contains(r'-d \"sokol_pipe=a|b\"')
	assert vopts.contains(r'-d \"sokol_percent=^%TEMP^%\"')
}

fn test_live_reload_forwarded_define_vopts_omits_unrelated_defines() {
	vopts := live_reload_forwarded_define_vopts(pref.OS.linux, [
		'sokol_d3d11',
		'windows',
		'livemain',
		'sharedlive',
		'gg_record',
		'custom',
	], {
		'sokol_d3d11': 'true'
		'windows':     'true'
		'livemain':    'true'
		'sharedlive':  'true'
		'gg_record':   'true'
		'custom':      'true'
	})
	assert vopts == "-d 'sokol_d3d11'"
}

fn test_live_reload_forwarded_define_vopts_keeps_allowed_non_sokol_defines() {
	vopts := live_reload_forwarded_define_vopts(pref.OS.linux, [
		'darwin_sokol_glcore33',
		'no_sokol_app',
		'gg_multiwindow',
	], {
		'darwin_sokol_glcore33': 'true'
		'no_sokol_app':          'true'
		'gg_multiwindow':        'true'
	})
	assert vopts == "-d 'darwin_sokol_glcore33' -d 'no_sokol_app' -d 'gg_multiwindow'"
}

fn test_live_reload_forwarded_define_vopts_forwards_gg_multiwindow_once_without_live_mode_flags() {
	vopts := live_reload_forwarded_define_vopts(pref.OS.linux, [
		'gg_multiwindow',
		'gg_multiwindow',
		'livemain',
		'sharedlive',
	], {
		'gg_multiwindow': 'true'
		'livemain':       'true'
		'sharedlive':     'true'
	})
	assert vopts == "-d 'gg_multiwindow'"
	assert !vopts.contains('livemain')
	assert !vopts.contains('sharedlive')
}

fn test_live_reload_forwarded_define_vopts_collapses_duplicates() {
	vopts := live_reload_forwarded_define_vopts(pref.OS.linux, [
		'sokol_d3d11',
		'sokol_d3d11',
		'sokol_x',
		'sokol_x',
	], {
		'sokol_d3d11': 'true'
		'sokol_x':     '1'
	})
	assert vopts == "-d 'sokol_d3d11' -d 'sokol_x=1'"
}
