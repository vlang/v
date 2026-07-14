module c

import v3.pref

fn test_c_directive_targets_use_requested_platform() {
	target := pref.target_from('macos', 'arm64') or { panic(err) }
	assert c_flag_args('macos -DMACOS', '', '', target) == ['-DMACOS']
	assert c_flag_args('arm64 -DARM64', '', '', target) == ['-DARM64']
	assert c_flag_args('linux -DLINUX', '', '', target).len == 0
	assert c_flag_args('amd64 -DAMD64', '', '', target).len == 0
	assert c_include_arg_for_target('macos <TargetConditionals.h>', '', '', target) == '<TargetConditionals.h>'
	assert c_include_arg_for_target('windows <windows.h>', '', '', target) == ''
}
