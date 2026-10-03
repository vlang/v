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

fn q(s string) string {
	return "'${s}'"
}

fn write_mod(vmodules string, name string, deps []string) {
	dir := os.join_path(vmodules, name.replace('.', os.path_separator))
	os.mkdir_all(dir) or { panic(err) }
	mut s := "Module {\n\tname: '${name}'\n"
	if deps.len > 0 {
		s += '\tdependencies: [' + deps.join(', ') + ',]\n'
	}
	s += '}\n'
	os.write_file(os.join_path(dir, 'v.mod'), s) or { panic(err) }
}

// new_project writes a project v.mod together with the modules it can see, and
// returns the directory to run `v why` from.
//
// Each project gets its own modules directory and points VMODULES at it, the way
// install_local_test.v does: the tool reads the variable when it starts, so setting
// it here is enough even though this process already resolved its own copy.
fn new_project(name string, root_deps []string, modules map[string][]string) string {
	vmodules := os.join_path(test_path, name, 'vmodules')
	project := os.join_path(test_path, name, 'myapp')
	test_utils.set_test_env(vmodules)
	os.mkdir_all(project) or { panic(err) }
	for mod, deps in modules {
		write_mod(vmodules, mod, deps)
	}
	mut s := "Module {\n\tname: '${name}'\n"
	if root_deps.len > 0 {
		s += '\tdependencies: [' + root_deps.join(', ') + ',]\n'
	}
	s += '}\n'
	os.write_file(os.join_path(project, 'v.mod'), s) or { panic(err) }
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
	return run_why(dir, args, false).output
}

fn testsuite_begin() {
	test_utils.set_test_env(test_path)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_whole_graph() {
	p := new_project('whole', [q('vsl'), q('markdown'), q('ghost')], {
		'vsl':      [q('c')]
		'markdown': []string{}
		'c':        []string{}
	})
	out := why_output(p, []string{})
	assert out.contains('whole'), out
	assert out.contains('vsl'), out
	assert out.contains('c'), out
	assert out.contains('markdown'), out
	// Required by the project but never installed: reported rather than dropped,
	// because that is the answer the command exists to give.
	assert out.contains('ghost (not installed)'), out
}

fn test_explains_a_transitive_module() {
	p := new_project('transitive', [q('vsl')], {
		'vsl': [q('c')]
		'c':   []string{}
	})
	out := why_output(p, ['c'])
	assert out.contains('vsl'), out
	assert out.contains('c'), out
	// the whole-graph view is not what was asked for
	assert !out.contains('markdown'), out
}

fn test_lists_every_route_to_one_module() {
	p := new_project('routes', [q('a'), q('b')], {
		'a':      [q('shared')]
		'b':      [q('shared')]
		'shared': []string{}
	})
	out := why_output(p, ['shared'])
	// Two independent routes, so the module appears once under each.
	assert out.count('shared') == 2, out
	assert out.contains('a'), out
	assert out.contains('b'), out
}

fn test_survives_a_dependency_cycle() {
	p := new_project('cycle', [q('cyc1')], {
		'cyc1': [q('cyc2')]
		'cyc2': [q('cyc3')]
		'cyc3': [q('cyc1')]
	})
	// The whole-graph view recurses through the loop. Without a guard this
	// overflows the stack instead of returning, which is how the omission was found.
	out := why_output(p, []string{})
	assert out.contains('(cycle)'), out
	assert out.contains('cyc3'), out

	out2 := why_output(p, ['cyc2'])
	assert out2.contains('cyc1'), out2
	assert out2.contains('cyc2'), out2
}

fn test_a_module_named_by_url_is_the_same_node() {
	p := new_project('urlform', [q('https://github.com/publisher/urlmod')], {
		'publisher/urlmod': []string{}
	})
	// written as a URL in the manifest, asked for as a registered name
	out := why_output(p, ['publisher.urlmod'])
	assert out.contains('publisher/urlmod'), out
	// and asked for as the URL itself
	out2 := why_output(p, ['https://github.com/publisher/urlmod'])
	assert out2.contains('publisher/urlmod'), out2
}

fn test_unknown_module_fails_with_a_pointer() {
	p := new_project('unknown', [q('markdown')], {
		'markdown': []string{}
	})
	res := run_why(p, ['nosuchmodule'], true)
	assert res.exit_code == 1, res.output
	assert res.output.contains('nosuchmodule'), res.output
	// the message names the graph it searched, so the reader knows what was searched
	assert res.output.contains('unknown'), res.output
}

fn test_missing_vmod_is_reported() {
	empty := os.join_path(test_path, 'novmod')
	os.mkdir_all(empty) or { panic(err) }
	res := run_why(empty, []string{}, true)
	assert res.exit_code == 1, res.output
	assert res.output.contains('v.mod'), res.output
}
