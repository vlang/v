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
	ensure_modules_for_tool_are_installed('vdoc', '', []string{}, false)!
	assert os.ls(markdown_dir)! == ['v.mod']
	// Tools without external dependencies install nothing.
	ensure_modules_for_tool_are_installed('vfmt', '', []string{}, false)!
	assert os.ls(vmodules)! == ['markdown']
}

fn write_markdown_module(root string) ! {
	markdown_dir := os.join_path(root, 'markdown')
	os.mkdir_all(markdown_dir)!
	os.write_file(os.join_path(markdown_dir, 'v.mod'), "Module {\n\tname: 'markdown'\n}\n")!
	os.write_file(os.join_path(markdown_dir, 'markdown.v'), 'module markdown\n')!
}

fn test_ensure_modules_for_tool_are_installed_searches_every_module_root() {
	base := os.join_path(os.vtmp_dir(), 'util_module_deps_roots_${os.getpid()}')
	os.rmdir_all(base) or {}
	first_root := os.join_path(base, 'first')
	later_root := os.join_path(base, 'later')
	path_root := os.join_path(base, 'path')
	os.mkdir_all(first_root)!
	write_markdown_module(later_root)!
	write_markdown_module(path_root)!
	old_vmodules := os.getenv_opt('VMODULES')
	old_vexe := os.getenv_opt('VEXE')
	// Installing a module runs `$VEXE retry -- git clone ...`. A missing VEXE makes every
	// install attempt fail at once, so this test never reaches the network.
	os.setenv('VEXE', os.join_path(base, 'missing_v'), true)
	defer {
		if value := old_vmodules {
			os.setenv('VMODULES', value, true)
		} else {
			os.unsetenv('VMODULES')
		}
		if value := old_vexe {
			os.setenv('VEXE', value, true)
		} else {
			os.unsetenv('VEXE')
		}
		os.rmdir_all(base) or {}
	}
	// A module in a later VMODULES root is used, even though the first root does not have it.
	os.setenv('VMODULES', [first_root, later_root].join(os.path_delimiter), true)
	ensure_modules_for_tool_are_installed('vdoc', '', []string{}, false)!
	assert os.ls(first_root)! == []
	// So is a module that only a `-path` root of the build has.
	os.setenv('VMODULES', first_root, true)
	ensure_modules_for_tool_are_installed('vdoc', '', [path_root], false)!
	assert os.ls(first_root)! == []
	// A module that no root has is installed, and a failed install says how to do it manually.
	ensure_modules_for_tool_are_installed('vdoc', '', []string{}, false) or {
		assert err.msg().contains('Install it with `v install markdown`')
		return
	}
	assert false, 'a module that no root has must be installed'
}

fn test_ensure_modules_for_tool_are_installed_searches_the_tool_project_and_its_parents() {
	base := os.join_path(os.vtmp_dir(), 'util_module_deps_project_${os.getpid()}')
	os.rmdir_all(base) or {}
	vmodules := os.join_path(base, 'vmodules')
	project := os.join_path(base, 'workspace', 'project')
	tool_source := os.join_path(project, 'cmd', 'tools', 'vdoc')
	os.mkdir_all(vmodules)!
	os.mkdir_all(tool_source)!
	os.write_file(os.join_path(project, 'v.mod'), "Module {\n\tname: 'project'\n}\n")!
	os.write_file(os.join_path(tool_source, 'vdoc.v'), 'module main\n\nimport markdown\n')!
	old_vmodules := os.getenv_opt('VMODULES')
	old_vexe := os.getenv_opt('VEXE')
	// As above: a missing VEXE makes every install attempt fail, without any network access.
	os.setenv('VEXE', os.join_path(base, 'missing_v'), true)
	os.setenv('VMODULES', vmodules, true)
	defer {
		if value := old_vmodules {
			os.setenv('VMODULES', value, true)
		} else {
			os.unsetenv('VMODULES')
		}
		if value := old_vexe {
			os.setenv('VEXE', value, true)
		} else {
			os.unsetenv('VEXE')
		}
		os.rmdir_all(base) or {}
	}
	// The compiler resolves a module in the tool's project root, like `<vroot>/markdown`.
	write_markdown_module(project)!
	ensure_modules_for_tool_are_installed('vdoc', tool_source, []string{}, false)!
	ensure_modules_for_tool_are_installed('vdoc', os.join_path(tool_source, 'vdoc.v'), []string{},
		false)!
	// It also resolves one that is checked out next to the project.
	os.rmdir_all(os.join_path(project, 'markdown'))!
	write_markdown_module(os.join_path(base, 'workspace'))!
	ensure_modules_for_tool_are_installed('vdoc', tool_source, []string{}, false)!
	assert os.ls(vmodules)! == []
}
