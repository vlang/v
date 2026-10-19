import os

const figr_vexe = @VEXE
const figr_tests_dir = os.dir(@FILE)
const figr_v3_dir = os.dir(figr_tests_dir)
const figr_vlib_dir = os.dir(figr_v3_dir)
const figr_v3_src = os.join_path(figr_v3_dir, 'v.v')

fn figr_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_rhs_capture_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.exec([figr_vexe, '-gc', 'none', '-path', '${figr_vlib_dir}' + '|@vlib|@vmodules', '-o',
			v3_bin, '${figr_v3_src}'])
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) whose `@if`
// guard declaration reads an outer name on its RHS — `@if marked := transform(item)` — must
// capture the OUTER `item` for the RHS, and keep the guard binding local to the guarded block.
//
// The guard binding used to be named `item` too (`@if item := transform(item)`), which pinned down
// that the RHS is walked before the LHS binding is recorded. 1fa660cb8d made that a `redefinition`
// error (the V1 rule), so the shadowing form now has to be rejected by the parser with a
// diagnostic in the template, never reach C.
fn test_template_if_guard_rhs_captures_shadowed_outer() {
	v3_bin := figr_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_rhs_capture_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'figr' }\n") or { panic(err) }
	// Guard-bind `marked` to a transform OF the outer `item`, then print the guard-local value.
	os.write_file(os.join_path(root, 't.html'), '@if marked := transform(item)\n@{marked}\n@end\n') or {
		panic(err)
	}
	source := "module main\n\nfn transform(s string) ?string {\n\treturn s + '!'\n}\n\nfn build(item string) string {\n\treturn ('[' + \$tmpl('t.html') + ']').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('outer'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_rhs_capture_bin_${pid}')
	compile := os.exec([v3_bin, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// The RHS read the captured outer `item` ('outer'); the guard bound its transform ('outer!').
	assert run.output.trim_space() == '[outer!]', run.output

	// A guard binding that shadows the outer `item` it reads is a V error reported in the template.
	os.write_file(os.join_path(root, 't.html'), '@if item := transform(item)\n@{item}\n@end\n') or {
		panic(err)
	}
	shadow := os.exec([v3_bin, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert shadow.exit_code != 0, shadow.output
	assert shadow.output.contains('t.html:1:5: error: redefinition of `item`'), shadow.output
	assert !shadow.output.contains('C compilation failed'), shadow.output
}
