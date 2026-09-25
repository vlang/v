module util

import os

fn test_external_modules_for_tool() {
	assert external_modules_for_tool('vdoc') == ['markdown']
	assert external_modules_for_tool('vfmt') == []
	assert external_modules_for_tool('') == []
}

fn test_ensure_modules_for_tool_are_installed_keeps_installed_modules() {
	vmodules := os.join_path(os.vtmp_dir(), 'util_module_deps_${os.getpid()}')
	os.rmdir_all(vmodules) or {}
	markdown_dir := os.join_path(vmodules, 'markdown')
	os.mkdir_all(markdown_dir)!
	os.write_file(os.join_path(markdown_dir, 'v.mod'), "Module {\n\tname: 'markdown'\n}\n")!
	old_vmodules := os.getenv_opt('VMODULES')
	os.setenv('VMODULES', vmodules, true)
	defer {
		if value := old_vmodules {
			os.setenv('VMODULES', value, true)
		} else {
			os.unsetenv('VMODULES')
		}
		os.rmdir_all(vmodules) or {}
	}
	// An already installed module must be reused as is, without cloning or updating it.
	ensure_modules_for_tool_are_installed('vdoc', false)!
	assert os.ls(markdown_dir)! == ['v.mod']
	// Tools without external dependencies install nothing.
	ensure_modules_for_tool_are_installed('vfmt', false)!
	assert os.ls(vmodules)! == ['markdown']
}
