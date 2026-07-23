import os

const figs_vexe = @VEXE
const figs_tests_dir = os.dir(@FILE)
const figs_v3_dir = os.dir(figs_tests_dir)
const figs_vlib_dir = os.dir(figs_v3_dir)
const figs_v3_src = os.join_path(figs_v3_dir, 'v3.v')

fn figs_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_var_scope_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${figs_vexe} -gc none -path "${figs_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${figs_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) that opens
// an `@if item := find(items)` guard, references `@{item}` inside the guarded block, and then
// references `@{item}` AFTER the guard must capture the OUTER `item`. The guard binding shadows
// the outer name only inside the branch, so the capture collector must scope the guard
// declaration: without a nested scope the guard var stays "declared", the post-guard use is
// treated as builder-local, and the generated closure omits `item` from its capture list,
// leaving the reference undefined.
fn test_template_if_guard_var_does_not_shadow_outer_capture_after_guard() {
	v3_bin := figs_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_var_scope_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'figs' }\n") or { panic(err) }
	// Guard-bind `item` to the first element, print it inside the block, then print the OUTER
	// `item` after the guard closes.
	os.write_file(os.join_path(root, 't.html'), '@if item := find(items)\n@{item}\n@end\n|@{item}\n') or {
		panic(err)
	}
	source := "module main\n\nfn find(items []string) ?string {\n\tif items.len > 0 {\n\t\treturn items[0]\n\t}\n\treturn none\n}\n\nfn build(item string, items []string) string {\n\treturn ('[' + \$tmpl('t.html') + ']').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('OUTER', ['a', 'b']))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_var_scope_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	// Guarded block printed the first element, then the OUTER `item` printed after the guard.
	assert run.output.trim_space() == '[a|OUTER]', run.output
}
