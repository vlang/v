import os

// A module that is neither in vlib nor in a module directory nor beside the importing
// file can still be a project checked out next to the one being built. The legacy
// builder finds it by walking up from the importing file; V3 stopped at the project
// root, so two sibling checkouts could not build against each other and every import
// of the neighbour failed with `cannot import module "..." (not found)`.

const sibling_module_vexe = @VEXE
const sibling_module_tests_dir = os.dir(@FILE)
const sibling_module_v3_dir = os.dir(sibling_module_tests_dir)
const sibling_module_vlib_dir = os.dir(sibling_module_v3_dir)
const sibling_module_v3_src = os.join_path(sibling_module_v3_dir, 'v.v')

fn sibling_module_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_sibling_module_compiler_${os.getpid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${sibling_module_vexe} -gc none -path "${sibling_module_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${sibling_module_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// Lay out two projects side by side, each with its own manifest, the way a checkout
// of a library next to the program that uses it looks on disk.
fn sibling_module_write_projects(root string, dependency_name string, importer_source string) string {
	os.rmdir_all(root) or {}
	dependency_dir := os.join_path(root, dependency_name)
	os.mkdir_all(dependency_dir) or { panic(err) }
	os.write_file(os.join_path(dependency_dir, 'v.mod'), "Module {\n\tname: '${dependency_name}'\n\tversion: '0.0.1'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dependency_dir, 'lib.v'), 'module ${dependency_name}

pub fn greeting() string {
	return "from ${dependency_name}"
}
') or { panic(err) }

	importer_dir := os.join_path(root, 'app')
	os.mkdir_all(importer_dir) or { panic(err) }
	os.write_file(os.join_path(importer_dir, 'v.mod'), "Module {\n\tname: 'app'\n\tversion: '0.0.1'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(importer_dir, 'main.v'), importer_source) or { panic(err) }
	return importer_dir
}

fn test_a_module_checked_out_beside_the_project_is_found() {
	v3_bin := sibling_module_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_sibling_module_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	importer_dir := sibling_module_write_projects(root, 'neighbourlib', 'module main

import neighbourlib

fn main() {
	println(neighbourlib.greeting())
}
')
	// A module directory that carries the name but holds no sources -- an install that
	// was removed, or a symlink whose target is gone -- is what the project really has
	// to get past: it is searched before any neighbour, and finding nothing usable
	// there is not the end of the search.
	empty_modules := os.join_path(root, 'emptymodules')
	os.mkdir_all(os.join_path(empty_modules, 'neighbourlib')) or { panic(err) }
	exe := os.join_path(root, 'app_prog')
	compile := os.execute('${v3_bin} -nocache -path "${empty_modules}|@vlib|@vmodules" ${importer_dir} -b c -o ${exe}')
	assert compile.exit_code == 0, compile.output
	res := os.execute(exe)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'from neighbourlib', res.output
}

// The walk goes all the way up, not one level: a file deep inside the project still
// reaches a module sitting beside the project root.
fn test_the_walk_reaches_past_more_than_one_level() {
	v3_bin := sibling_module_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_sibling_module_deep_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	sibling_module_write_projects(root, 'neighbourlib', 'module main

fn main() {
}
')
	// Move the program into a subdirectory of its own project, several levels down.
	deep_dir := os.join_path(root, 'app', 'cmd', 'tool')
	os.mkdir_all(deep_dir) or { panic(err) }
	os.rm(os.join_path(root, 'app', 'main.v')) or {}
	os.write_file(os.join_path(deep_dir, 'main.v'), 'module main

import neighbourlib

fn main() {
	println(neighbourlib.greeting())
}
') or { panic(err) }
	exe := os.join_path(root, 'deep_prog')
	compile := os.execute('${v3_bin} -nocache ${deep_dir} -b c -o ${exe}')
	assert compile.exit_code == 0, compile.output
	res := os.execute(exe)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'from neighbourlib', res.output
}
