import os

const gelse_vexe = @VEXE
const gelse_tests_dir = os.dir(@FILE)
const gelse_v3_dir = os.dir(gelse_tests_dir)
const gelse_vlib_dir = os.dir(gelse_v3_dir)
const gelse_v3_src = os.join_path(gelse_v3_dir, 'v.v')

fn gelse_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_else_scope_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.exec([gelse_vexe, '-gc', 'none', '-path', '${gelse_vlib_dir}' + '|@vlib|@vmodules',
			'-o', v3_bin, '${gelse_v3_src}'])
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A template lowered into an IIFE that opens an `@if found := find(items)` guard, references the
// guard binding in the then branch and the outer `@{item}` in the else branch. The guard binding
// is scoped to the then branch only, so the capture collector must walk the guard/then/else with
// separate declared scopes: the else branch has to capture the OUTER `item`.
//
// The guard binding used to be named `item` too, shadowing the outer name in the then branch.
// 1fa660cb8d made that a `redefinition` error (the V1 rule), so the shadowing form now has to be
// rejected by the parser with a diagnostic in the template, never reach C.
fn test_template_if_guard_var_does_not_shadow_outer_capture_in_else_branch() {
	v3_bin := gelse_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_else_scope_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'gelse' }\n") or { panic(err) }
	// Guard-bind `found` to the first element (then branch prints it); the else branch prints the
	// OUTER `item`.
	os.write_file(os.join_path(root, 't.html'),
		'@if found := find(items)\nthen:@{found}\n@else\nelse:@{item}\n@end\n') or { panic(err) }
	source := "module main\n\nfn find(items []string) ?string {\n\tif items.len > 0 {\n\t\treturn items[0]\n\t}\n\treturn none\n}\n\nfn build(item string, items []string) string {\n\treturn ('[' + \$tmpl('t.html') + ']').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('OUTER', ['a', 'b']))\n\tprintln(build('OUTER', []string{}))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_else_scope_bin_${pid}')
	compile := os.exec([v3_bin, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().split('\n')
	assert lines.len == 2, run.output
	// Non-empty items: the guard succeeds, the then branch prints the guard-bound first element.
	assert lines[0] == '[then:a]', run.output
	// Empty items: the guard fails, the else branch prints the OUTER `item` (captured).
	assert lines[1] == '[else:OUTER]', run.output

	// A guard binding that shadows the outer `item` is a V error reported in the template.
	os.write_file(os.join_path(root, 't.html'),
		'@if item := find(items)\nthen:@{item}\n@else\nelse:@{item}\n@end\n') or { panic(err) }
	shadow := os.exec([v3_bin, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert shadow.exit_code != 0, shadow.output
	assert shadow.output.contains('t.html:1:5: error: redefinition of `item`'), shadow.output
	assert !shadow.output.contains('C compilation failed'), shadow.output
}
