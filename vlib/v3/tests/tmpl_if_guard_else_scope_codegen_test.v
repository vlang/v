import os

const gelse_vexe = @VEXE
const gelse_tests_dir = os.dir(@FILE)
const gelse_v3_dir = os.dir(gelse_tests_dir)
const gelse_vlib_dir = os.dir(gelse_v3_dir)
const gelse_v3_src = os.join_path(gelse_v3_dir, 'v3.v')

fn gelse_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_else_scope_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${gelse_vexe} -gc none -path "${gelse_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${gelse_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A template lowered into an IIFE that opens an `@if item := find(items)` guard and references
// `@{item}` in BOTH the then and the else branch. In V the guard binding is scoped to the then
// branch only, so the `@{item}` in the else branch must capture the OUTER `item`. The capture
// collector must therefore walk the guard/then/else with separate declared scopes and drop the
// guard binding before the else branch: without that, the else use is treated as guard-local,
// the closure omits `item` from its capture list, and the else reference is left undefined (or
// bound to the wrong symbol), so the guard binding — unbound in the else branch — is what the
// generated code reaches for.
fn test_template_if_guard_var_does_not_shadow_outer_capture_in_else_branch() {
	v3_bin := gelse_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_else_scope_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'gelse' }\n") or { panic(err) }
	// Guard-bind `item` to the first element (then branch prints it); the else branch prints the
	// OUTER `item`, which is NOT the guard binding.
	os.write_file(os.join_path(root, 't.html'),
		'@if item := find(items)\nthen:@{item}\n@else\nelse:@{item}\n@end\n') or { panic(err) }
	source := "module main\n\nfn find(items []string) ?string {\n\tif items.len > 0 {\n\t\treturn items[0]\n\t}\n\treturn none\n}\n\nfn build(item string, items []string) string {\n\treturn ('[' + \$tmpl('t.html') + ']').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('OUTER', ['a', 'b']))\n\tprintln(build('OUTER', []string{}))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_else_scope_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().split('\n')
	assert lines.len == 2, run.output
	// Non-empty items: the guard succeeds, the then branch prints the guard-bound first element.
	assert lines[0] == '[then:a]', run.output
	// Empty items: the guard fails, the else branch prints the OUTER `item` (captured), not the
	// unbound guard variable.
	assert lines[1] == '[else:OUTER]', run.output
}
