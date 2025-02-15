module document

import os

fn testsuite_begin() {
	os.chdir(@VMODROOT) or {}
	eprintln('>> @VMODROOT: ' + @VMODROOT)
}

fn test_get_parent_mod_on_root_folder() {
	// TODO: add an equivalent windows check for c:\
	$if !windows {
		assert '---' == get_parent_mod('/') or {
			assert err.msg() == 'root folder reached'
			'---'
		}
	}
}

fn test_get_parent_mod_current_folder() {
	// TODO: this should may be return '' reliably on windows too:
	// assert '' == get_parent_mod('.') or {
	//	assert err.msg() == 'No V files found.'
	//	'---'
	// }
}

fn test_get_parent_mod_on_temp_dir() {
	// TODO: fix this on windows
	$if !windows {
		assert get_parent_mod(os.temp_dir())? == ''
	}
}

fn test_get_parent_mod_normal_cases() {
	assert '---' == get_parent_mod(os.join_path(@VMODROOT, 'vlib/v')) or {
		assert err.msg() == 'No V files found.'
		'---'
	}
	assert get_parent_mod(os.join_path(@VMODROOT, 'vlib', 'v', 'token'))? == 'v'
	assert get_parent_mod(os.join_path(@VMODROOT, 'vlib', 'os', 'os.v'))? == 'os'
	assert get_parent_mod(os.join_path(@VMODROOT, 'cmd'))? == ''
	assert get_parent_mod(os.join_path(@VMODROOT, 'cmd', 'tools', 'modules', 'testing',
		'common.v'))? == 'tools.modules.testing'
}
