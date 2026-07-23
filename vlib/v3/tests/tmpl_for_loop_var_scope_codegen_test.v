import os

const flvs_vexe = @VEXE
const flvs_tests_dir = os.dir(@FILE)
const flvs_v3_dir = os.dir(flvs_tests_dir)
const flvs_vlib_dir = os.dir(flvs_v3_dir)
const flvs_v3_src = os.join_path(flvs_v3_dir, 'v3.v')

fn flvs_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_for_loop_var_scope_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${flvs_vexe} -gc none -path "${flvs_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${flvs_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) that loops
// with a `@for item in items` and then references `@{item}` AFTER the loop must capture the
// OUTER `item`. The loop variable shadows the outer name only inside the loop body, so the
// capture collector must scope the loop declaration: without a nested scope the loop var stays
// "declared", the post-loop use is treated as builder-local, and the generated closure omits
// `item` from its capture list, leaving the reference undefined.
fn test_template_loop_var_does_not_shadow_outer_capture_after_loop() {
	v3_bin := flvs_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_for_loop_var_scope_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'flvs' }\n") or { panic(err) }
	// Loop over `items` printing each element, then print the OUTER `item` after the loop.
	os.write_file(os.join_path(root, 't.html'), '@for item in items\n@{item}\n@end\n|@{item}\n') or {
		panic(err)
	}
	source := "module main\n\nfn build(item string, items []string) string {\n\treturn ('[' + \$tmpl('t.html') + ']').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('OUTER', ['a', 'b']))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_for_loop_var_scope_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	// Loop body concatenated the two elements, then the OUTER `item` printed after the loop.
	assert run.output.trim_space() == '[ab|OUTER]', run.output
}
