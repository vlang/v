import os

const flvs_vexe = @VEXE
const flvs_tests_dir = os.dir(@FILE)
const flvs_v3_dir = os.dir(flvs_tests_dir)
const flvs_vlib_dir = os.dir(flvs_v3_dir)
const flvs_v3_src = os.join_path(flvs_v3_dir, 'v.v')

fn flvs_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_for_loop_var_scope_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.exec([flvs_vexe, '-gc', 'none', '-path', '${flvs_vlib_dir}' + '|@vlib|@vmodules', '-o',
			v3_bin, '${flvs_v3_src}'])
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) that loops
// with `@for row in items` and then references the outer `@{item}` AFTER the loop must keep the
// loop variable local to the loop and capture the OUTER `item`: the capture collector scopes the
// loop declaration, so `row` is never captured and the post-loop `item` is.
//
// The loop variable used to be named `item` too, shadowing the outer name. 5c44a423cf made that
// a `redefinition of value iteration variable` error (the V1 rule), so the shadowing form now
// has to be rejected by the parser with a diagnostic in the template, never reach C.
fn test_template_loop_var_does_not_shadow_outer_capture_after_loop() {
	v3_bin := flvs_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_for_loop_var_scope_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'flvs' }\n") or { panic(err) }
	// Loop over `items` printing each element, then print the OUTER `item` after the loop.
	os.write_file(os.join_path(root, 't.html'), '@for row in items\n@{row}\n@end\n|@{item}\n') or {
		panic(err)
	}
	source := "module main\n\nfn build(item string, items []string) string {\n\treturn ('[' + \$tmpl('t.html') + ']').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('OUTER', ['a', 'b']))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_for_loop_var_scope_bin_${pid}')
	compile := os.exec([v3_bin, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// Loop body concatenated the two elements, then the OUTER `item` printed after the loop.
	assert run.output.trim_space() == '[ab|OUTER]', run.output

	// A loop variable that shadows the outer `item` is a V error reported in the template.
	os.write_file(os.join_path(root, 't.html'), '@for item in items\n@{item}\n@end\n|@{item}\n') or {
		panic(err)
	}
	shadow := os.exec([v3_bin, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert shadow.exit_code != 0, shadow.output
	assert shadow.output.contains('t.html:1:6: error: redefinition of value iteration variable `item`'), shadow.output
	assert !shadow.output.contains('C compilation failed'), shadow.output
}
