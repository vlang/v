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

// An import written `foo.bar` is the directory `foo/bar`, and the manifest naming the
// project sits above it in `foo`. Asking only the leaf directory for a manifest finds
// none, which would make a submodule of a neighbour look like a stranger.
fn test_a_dotted_submodule_of_a_neighbour_is_found() {
	v3_bin := sibling_module_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_sibling_module_dotted_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	os.rmdir_all(root) or {}
	dependency_dir := os.join_path(root, 'neighbourlib')
	submodule_dir := os.join_path(dependency_dir, 'inner')
	os.mkdir_all(submodule_dir) or { panic(err) }
	os.write_file(os.join_path(dependency_dir, 'v.mod'), "Module {\n\tname: 'neighbourlib'\n\tversion: '0.0.1'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(submodule_dir, 'inner.v'), 'module inner

pub fn deeper() string {
	return "from the submodule"
}
') or { panic(err) }

	importer_dir := os.join_path(root, 'app')
	os.mkdir_all(importer_dir) or { panic(err) }
	os.write_file(os.join_path(importer_dir, 'v.mod'), "Module {\n\tname: 'app'\n\tversion: '0.0.1'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(importer_dir, 'main.v'), 'module main

import neighbourlib.inner

fn main() {
	println(inner.deeper())
}
') or { panic(err) }

	empty_modules := os.join_path(root, 'emptymodules')
	os.mkdir_all(empty_modules) or { panic(err) }
	exe := os.join_path(root, 'dotted_prog')
	compile := os.execute('${v3_bin} -nocache -path "${empty_modules}|@vlib|@vmodules" ${importer_dir} -b c -o ${exe}')
	assert compile.exit_code == 0, compile.output
	res := os.execute(exe)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'from the submodule', res.output
}

// A project's own dependency wins over a neighbour of the project that happens to
// carry the same name. The walk reaches the project root, where the project's own
// copy sits, before it reaches what is beside the project, so a source file buried
// in the project still gets the project's own copy.
fn test_the_projects_own_modules_beat_a_neighbour_of_the_same_name() {
	v3_bin := sibling_module_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_sibling_module_own_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	os.rmdir_all(root) or {}

	// A neighbour of the project, carrying the name.
	neighbour_dir := os.join_path(root, 'shared')
	os.mkdir_all(neighbour_dir) or { panic(err) }
	os.write_file(os.join_path(neighbour_dir, 'v.mod'), "Module {\n\tname: 'shared'\n\tversion: '0.0.1'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(neighbour_dir, 'lib.v'), 'module shared

pub fn who() string {
	return "the neighbour"
}
') or { panic(err) }

	// The project, with its own copy in its lookup root, and its source a level down.
	app_dir := os.join_path(root, 'app')
	own_dir := os.join_path(app_dir, 'shared')
	src_dir := os.join_path(app_dir, 'src')
	os.mkdir_all(own_dir) or { panic(err) }
	os.mkdir_all(src_dir) or { panic(err) }
	os.write_file(os.join_path(app_dir, 'v.mod'), "Module {\n\tname: 'app'\n\tversion: '0.0.1'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(own_dir, 'lib.v'), 'module shared

pub fn who() string {
	return "the project own copy"
}
') or { panic(err) }
	os.write_file(os.join_path(src_dir, 'main.v'), 'module main

import shared

fn main() {
	println(shared.who())
}
') or { panic(err) }

	empty_modules := os.join_path(root, 'emptymodules')
	os.mkdir_all(empty_modules) or { panic(err) }
	exe := os.join_path(root, 'own_prog')
	compile := os.execute('${v3_bin} -nocache -path "${empty_modules}|@vlib|@vmodules" ${src_dir} -b c -o ${exe}')
	assert compile.exit_code == 0, compile.output
	res := os.execute(exe)
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'the project own copy', res.output
}

// A `modules` directory is a lookup root for nobody, its own files included. What
// it holds is `modules.<name>`, so a file in one looks past it for a bare name --
// out to the project and its neighbours, as any other file would. The old layout
// does not survive between the very modules that have to move out of it.
fn test_a_modules_directory_is_not_a_lookup_root_for_the_files_in_it() {
	v3_bin := sibling_module_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_modules_not_a_root_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	project_dir := os.join_path(root, 'app')
	foo_dir := os.join_path(project_dir, 'modules', 'foo')
	nested_bar_dir := os.join_path(project_dir, 'modules', 'bar')
	neighbour_bar_dir := os.join_path(root, 'bar')
	os.mkdir_all(foo_dir) or { panic(err) }
	os.mkdir_all(nested_bar_dir) or { panic(err) }
	os.mkdir_all(neighbour_bar_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module {\n\tname: 'app'\n\tversion: '0.0.1'\n}\n") or {
		panic(err)
	}
	// Two modules a file in `modules/foo` could mean by `bar`: its neighbour in
	// that directory, and the project checked out beside the one being built.
	os.write_file(os.join_path(nested_bar_dir, 'bar.v'), 'module bar

pub fn value() string {
	return "from the modules directory"
}
') or { panic(err) }
	os.write_file(os.join_path(neighbour_bar_dir, 'v.mod'), "Module {\n\tname: 'bar'\n\tversion: '0.0.1'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(neighbour_bar_dir, 'bar.v'), 'module bar

pub fn value() string {
	return "from the neighbour project"
}
') or { panic(err) }
	os.write_file(os.join_path(project_dir, 'main.v'), 'module main

import modules.foo

fn main() {
	println(foo.value())
}
') or { panic(err) }

	empty_modules := os.join_path(root, 'emptymodules')
	os.mkdir_all(empty_modules) or { panic(err) }
	exe := os.join_path(root, 'modules_root_prog')

	// A bare `bar` climbs out of `modules/` without stopping in it, and reaches the
	// neighbour -- the directory next to `foo` is not what the name means.
	os.write_file(os.join_path(foo_dir, 'foo.v'), 'module foo

import bar

pub fn value() string {
	return bar.value()
}
') or { panic(err) }
	bare := os.execute('${v3_bin} -nocache -path "${empty_modules}|@vlib|@vmodules" ${project_dir} -b c -o ${exe}')
	assert bare.exit_code == 0, bare.output
	bare_run := os.execute(exe)
	assert bare_run.exit_code == 0, bare_run.output
	assert bare_run.output.trim_space() == 'from the neighbour project', bare_run.output

	// The neighbour in `modules/` is reached by the name the layout gives it.
	os.write_file(os.join_path(foo_dir, 'foo.v'), 'module foo

import modules.bar

pub fn value() string {
	return bar.value()
}
') or { panic(err) }
	dotted := os.execute('${v3_bin} -nocache -path "${empty_modules}|@vlib|@vmodules" ${project_dir} -b c -o ${exe}')
	assert dotted.exit_code == 0, dotted.output
	dotted_run := os.execute(exe)
	assert dotted_run.exit_code == 0, dotted_run.output
	assert dotted_run.output.trim_space() == 'from the modules directory', dotted_run.output
}
