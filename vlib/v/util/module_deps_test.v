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
	ensure_modules_for_tool_are_installed('vdoc', '', false)!
	assert os.ls(markdown_dir)! == ['v.mod']
	// Tools without external dependencies install nothing.
	ensure_modules_for_tool_are_installed('vfmt', '', false)!
	assert os.ls(vmodules)! == ['markdown']
}

fn write_markdown_module(root string) ! {
	markdown_dir := os.join_path(root, 'markdown')
	os.mkdir_all(markdown_dir)!
	os.write_file(os.join_path(markdown_dir, 'v.mod'), "Module {\n\tname: 'markdown'\n}\n")!
	os.write_file(os.join_path(markdown_dir, 'markdown.v'), 'module markdown\n')!
}

fn restore_env(name string, value ?string) {
	if v := value {
		os.setenv(name, v, true)
	} else {
		os.unsetenv(name)
	}
}

// install_is_attempted reports whether the `markdown` module of `vdoc` had to be installed,
// since it could not be found. The tests point VEXE at a missing program, so that every
// install attempt fails at once, and the network is never reached.
fn install_is_attempted(tool_source string) bool {
	ensure_modules_for_tool_are_installed('vdoc', tool_source, false) or {
		assert err.msg().contains('Install it with `v install markdown`')
		return true
	}
	return false
}

fn test_ensure_modules_for_tool_are_installed_searches_every_module_root() {
	base := os.join_path(os.vtmp_dir(), 'util_module_deps_roots_${os.getpid()}')
	os.rmdir_all(base) or {}
	first_root := os.join_path(base, 'first')
	later_root := os.join_path(base, 'later')
	os.mkdir_all(first_root)!
	old_vmodules := os.getenv_opt('VMODULES')
	old_vexe := os.getenv_opt('VEXE')
	os.setenv('VEXE', os.join_path(base, 'missing_v'), true)
	os.setenv('VMODULES', [first_root, later_root].join(os.path_delimiter), true)
	defer {
		restore_env('VMODULES', old_vmodules)
		restore_env('VEXE', old_vexe)
		os.rmdir_all(base) or {}
	}
	// A module that no root has is installed, and a failed install says how to do it manually.
	assert install_is_attempted('')
	// A module in a later VMODULES root is used, even though the first root does not have it.
	write_markdown_module(later_root)!
	assert !install_is_attempted('')
	// So is a package with a `v.mod`, that keeps its sources elsewhere, like in `src/`.
	os.rmdir_all(os.join_path(later_root, 'markdown'))!
	package_dir := os.join_path(later_root, 'markdown')
	os.mkdir_all(os.join_path(package_dir, 'src'))!
	os.write_file(os.join_path(package_dir, 'v.mod'), "Module {\n\tname: 'markdown'\n\tbase_url: 'src'\n}\n")!
	os.write_file(os.join_path(package_dir, 'src', 'markdown.v'), 'module markdown\n')!
	assert !install_is_attempted('')
	assert os.ls(first_root)! == []
}

fn test_ensure_modules_for_tool_are_installed_searches_the_tool_project_and_its_parents() {
	base := os.join_path(os.vtmp_dir(), 'util_module_deps_project_${os.getpid()}')
	os.rmdir_all(base) or {}
	vmodules := os.join_path(base, 'vmodules')
	workspace := os.join_path(base, 'workspace')
	project := os.join_path(workspace, 'project')
	tool_source := os.join_path(project, 'cmd', 'tools', 'vdoc')
	os.mkdir_all(vmodules)!
	os.mkdir_all(tool_source)!
	os.write_file(os.join_path(project, 'v.mod'), "Module {\n\tname: 'project'\n}\n")!
	os.write_file(os.join_path(tool_source, 'vdoc.v'), 'module main\n\nimport markdown\n')!
	old_vmodules := os.getenv_opt('VMODULES')
	old_vexe := os.getenv_opt('VEXE')
	os.setenv('VEXE', os.join_path(base, 'missing_v'), true)
	os.setenv('VMODULES', vmodules, true)
	defer {
		restore_env('VMODULES', old_vmodules)
		restore_env('VEXE', old_vexe)
		os.rmdir_all(base) or {}
	}
	// The compiler resolves a module in the tool's project root, like `<vroot>/markdown`.
	write_markdown_module(project)!
	assert !install_is_attempted(tool_source)
	assert !install_is_attempted(os.join_path(tool_source, 'vdoc.v'))
	// It also resolves one that is checked out next to the project.
	os.rmdir_all(os.join_path(project, 'markdown'))!
	write_markdown_module(workspace)!
	assert !install_is_attempted(tool_source)
	assert os.ls(vmodules)! == []
}

fn test_ensure_modules_for_tool_are_installed_accepts_a_module_installed_concurrently() {
	$if windows {
		return
	}
	base := os.join_path(os.vtmp_dir(), 'util_module_deps_concurrent_${os.getpid()}')
	os.rmdir_all(base) or {}
	vmodules := os.join_path(base, 'vmodules')
	os.mkdir_all(vmodules)!
	// This stands in for `v retry -- git clone <url> <destination>`, while another `v` process
	// installs the module first: the destination gets populated, and this clone fails, like a
	// `git clone` into a folder that is not empty does.
	fake_vexe := os.join_path(base, 'fake_v')
	os.write_file(fake_vexe, '#!/bin/sh\nmkdir -p "\$6" && echo "module markdown" > "\$6/markdown.v"\nexit 128\n')!
	os.chmod(fake_vexe, 0o755)!
	old_vmodules := os.getenv_opt('VMODULES')
	old_vexe := os.getenv_opt('VEXE')
	os.setenv('VEXE', fake_vexe, true)
	os.setenv('VMODULES', vmodules, true)
	defer {
		restore_env('VMODULES', old_vmodules)
		restore_env('VEXE', old_vexe)
		os.rmdir_all(base) or {}
	}
	ensure_modules_for_tool_are_installed('vdoc', '', false)!
	assert os.is_file(os.join_path(vmodules, 'markdown', 'markdown.v'))
}
