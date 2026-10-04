module main

import os
import rand
import test_utils { cmd_fail_args, cmd_ok_args }

// Not os.vtmp_dir(): set_test_env points VTMP inside this tree, and VTMP is the
// compiler's own build directory. The fixtures would then live inside the scratch
// space that `v why` re-enters on every invocation, which made the run
// intermittently fail with "no v.mod found" for a file that was demonstrably there.
const test_path = os.join_path(os.temp_dir(), 'vpm_why_test_${rand.ulid()}')

// Not `vexe`: that name is already declared by the other test files in this
// directory, and a file that only works when compiled alongside them is a file that
// stops working when someone runs it on its own.
//
// Not `os.quoted_path(@VEXE)` either. cmd_ok_args passes its array straight to
// os.exec, so a path carrying quotes reaches CreateProcess with the quotes still
// attached and comes back as "Access is denied".
const why_exe = @VEXE

// The fixtures are plain directories holding a v.mod. Nothing installs them and
// nothing runs git, because `v why` only reads v.mod files. VMODULES is redirected
// through the environment, which is the only way to point a subprocess at them.

// Fixture is one installed module. `path` is where it sits under its root,
// written as an import path (`nedpals.args`). `name` is what its own v.mod
// declares, which for a real package is usually only the last part (`args`).
// A fixture without a `name` has no v.mod at all.
struct Fixture {
	path string
	name string
	deps []string
}

fn fixture(path string, name string, deps ...string) Fixture {
	return Fixture{
		path: path
		name: name
		deps: deps
	}
}

fn q(s string) string {
	return "'${s}'"
}

fn write_mod(root string, f Fixture) {
	dir := os.join_path(root, f.path.replace('.', os.path_separator))
	os.mkdir_all(dir) or { panic(err) }
	if f.name == '' {
		mod_name := f.path.all_after_last('.')
		os.write_file(os.join_path(dir, '${mod_name}.v'), 'module ${mod_name}\n') or {
			panic(err)
		}
		return
	}
	mut s := "Module {\n\tname: '${f.name}'\n"
	if f.deps.len > 0 {
		s += '\tdependencies: [' + f.deps.map(q).join(', ') + ',]\n'
	}
	s += '}\n'
	os.write_file(os.join_path(dir, 'v.mod'), s) or { panic(err) }
}

// new_project writes a project v.mod together with the modules it can see, and
// returns the directory to run `v why` from. `local` modules are written into the
// project's own folder, where `v install --local` puts them.
//
// Each project gets its own modules directory and points VMODULES at it, the way
// install_local_test.v does: the tool reads the variable when it starts, so setting
// it here is enough even though this process already resolved its own copy.
fn new_project(name string, root_deps []string, modules []Fixture, local []Fixture) string {
	vmodules := os.join_path(test_path, name, 'vmodules')
	project := os.join_path(test_path, name, 'myapp')
	test_utils.set_test_env(vmodules)
	os.mkdir_all(project) or { panic(err) }
	for f in modules {
		write_mod(vmodules, f)
	}
	for f in local {
		write_mod(project, f)
	}
	// The project is written the same way, at the root of its own folder.
	write_mod(project, Fixture{
		path: ''
		name: name
		deps: root_deps
	})
	return project
}

fn why_args(args []string) []string {
	mut all := [why_exe, 'why']
	all << args
	return all
}

// run_why runs `v why` inside `dir`. `expect_failure` picks the assertion helper,
// because a command that is expected to fail must not be checked for success.
fn run_why(dir string, args []string, expect_failure bool) os.Result {
	old := os.getwd()
	os.chdir(dir) or { panic(err) }
	res := if expect_failure {
		cmd_fail_args(@LOCATION, why_args(args))
	} else {
		cmd_ok_args(@LOCATION, why_args(args))
	}
	os.chdir(old) or { panic(err) }
	return res
}

fn why_output(dir string, args []string) string {
	return run_why(dir, args, false).output.replace('\r\n', '\n')
}

fn tree(lines ...string) string {
	return lines.join('\n') + '\n'
}

fn testsuite_begin() {
	test_utils.set_test_env(test_path)
	// With CI set, vpm logs to stderr instead of to its log file, and os.exec
	// returns stderr together with stdout, which the exact comparisons below cannot
	// tell apart from the tree.
	os.unsetenv('CI')
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_whole_graph() {
	p := new_project('whole', ['vsl', 'markdown', 'ghost'], [
		fixture('vsl', 'vsl', 'c'),
		fixture('markdown', 'markdown'),
		fixture('c', 'c', 'nedpals.args'),
		fixture('nedpals.args', 'args'),
	], [])
	// A module is shown by its import path, not by the `name` its v.mod declares.
	// Required by the project but never installed: reported rather than dropped,
	// because that is the answer the command exists to give.
	assert why_output(p, []) == tree('whole', '  |-- vsl', '  |   `-- c',
		'  |       `-- nedpals.args', '  |-- markdown', '  `-- ghost (not installed)')
	assert why_output(p, ['ghost']) == tree('whole', '  `-- ghost (not installed)')
}

fn test_explains_a_transitive_module() {
	p := new_project('transitive', ['vsl', 'markdown'], [
		fixture('vsl', 'vsl', 'c'),
		fixture('markdown', 'markdown'),
		fixture('c', 'c', 'nedpals.args'),
		fixture('nedpals.args', 'args'),
	], [])
	// Only the route to the module: `markdown` does not lead to it, and is left out.
	assert why_output(p, ['nedpals.args']) == tree('transitive', '  `-- vsl', '      `-- c',
		'          `-- nedpals.args')
}

fn test_lists_every_route_to_one_module() {
	p := new_project('routes', ['a', 'b', 'unrelated'], [
		fixture('a', 'a', 'shared'),
		fixture('b', 'b', 'shared'),
		fixture('shared', 'shared'),
		fixture('unrelated', 'unrelated'),
	], [])
	// Two independent routes, so the module appears once under each. `b` is the
	// last route drawn, even though `unrelated` follows it in the v.mod.
	assert why_output(p, ['shared']) == tree('routes', '  |-- a', '  |   `-- shared', '  `-- b',
		'      `-- shared')
}

fn test_survives_a_dependency_cycle() {
	p := new_project('cycle', ['cyc1'], [
		fixture('cyc1', 'cyc1', 'cyc2'),
		fixture('cyc2', 'cyc2', 'cyc3'),
		fixture('cyc3', 'cyc3', 'cyc1'),
	], [])
	// The whole-graph view recurses through the loop. Without a guard this
	// overflows the stack instead of returning, which is how the omission was found.
	assert why_output(p, []) == tree('cycle', '  `-- cyc1', '      `-- cyc2', '          `-- cyc3',
		'              `-- cyc1 (cycle)')
	assert why_output(p, ['cyc2']) == tree('cycle', '  `-- cyc1', '      `-- cyc2')
}

fn test_a_module_named_by_url_is_the_same_node() {
	p := new_project('urlform', ['https://github.com/publisher/urlmod'], [
		fixture('publisher.urlmod', 'urlmod'),
	], [])
	expected := tree('urlform', '  `-- publisher.urlmod')
	assert why_output(p, []) == expected
	// written as a URL in the manifest, asked for as a registered name
	assert why_output(p, ['publisher.urlmod']) == expected
	// and asked for as the URL itself
	assert why_output(p, ['https://github.com/publisher/urlmod']) == expected
}

fn test_modules_with_the_same_manifest_name_stay_apart() {
	p := new_project('collision', ['alice.utils', 'bob.utils'], [
		fixture('alice.utils', 'utils'),
		fixture('bob.utils', 'utils', 'bob.deep'),
		fixture('bob.deep', 'deep'),
	], [])
	assert why_output(p, []) == tree('collision', '  |-- alice.utils', '  `-- bob.utils',
		'      `-- bob.deep')
	assert why_output(p, ['bob.deep']) == tree('collision', '  `-- bob.utils', '      `-- bob.deep')
}

fn test_a_module_without_a_manifest_is_a_node() {
	p := new_project('manifestless', ['nomanifest', 'markdown'], [
		fixture('nomanifest', ''),
		fixture('markdown', 'markdown'),
	], [])
	assert why_output(p, []) == tree('manifestless', '  |-- nomanifest', '  `-- markdown')
	assert why_output(p, ['nomanifest']) == tree('manifestless', '  `-- nomanifest')
}

fn test_finds_modules_installed_in_the_project() {
	p := new_project('local', ['localdep', 'markdown'], [
		fixture('markdown', 'markdown'),
	], [
		fixture('localdep', 'localdep', 'markdown'),
	])
	assert why_output(p, []) == tree('local', '  |-- localdep', '  |   `-- markdown',
		'  `-- markdown')
	assert why_output(p, ['localdep']) == tree('local', '  `-- localdep')
}

fn test_rejects_more_than_one_module() {
	p := new_project('toomany', ['a', 'b'], [
		fixture('a', 'a'),
		fixture('b', 'b'),
	], [])
	old := os.getwd()
	os.chdir(p) or { panic(err) }
	res := os.exec(why_args(['a', 'b']))
	os.chdir(old) or { panic(err) }
	assert res.exit_code == 2, res.output
	assert res.output.contains('at most one module name'), res.output
}

fn test_unknown_module_fails_with_a_pointer() {
	p := new_project('unknown', ['markdown'], [
		fixture('markdown', 'markdown'),
	], [])
	res := run_why(p, ['nosuchmodule'], true)
	assert res.exit_code == 1, res.output
	assert res.output.contains('`nosuchmodule` is not in the dependency graph of `unknown`.'), res.output
}

fn test_missing_vmod_is_reported() {
	empty := os.join_path(test_path, 'novmod')
	os.mkdir_all(empty) or { panic(err) }
	res := run_why(empty, []string{}, true)
	assert res.exit_code == 1, res.output
	assert res.output.contains('no v.mod found'), res.output
}
