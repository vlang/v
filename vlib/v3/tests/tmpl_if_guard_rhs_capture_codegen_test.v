import os

const figr_vexe = @VEXE
const figr_tests_dir = os.dir(@FILE)
const figr_v3_dir = os.dir(figr_tests_dir)
const figr_vlib_dir = os.dir(figr_v3_dir)
const figr_v3_src = os.join_path(figr_v3_dir, 'v3.v')

fn figr_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_rhs_capture_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${figr_vexe} -gc none -path "${figr_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${figr_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) whose `@if`
// guard declaration reuses a shadowed outer name on its RHS — `@if item := transform(item)` — must
// capture the OUTER `item` for the RHS. The declaration binds `item` for the guarded block, but the
// RHS `transform(item)` reads the outer value first; if the capture collector records the LHS
// binding before walking the RHS, the RHS `item` is treated as builder-local, so the closure omits
// the required capture and the generated IIFE references an undefined `item` (fails to compile).
fn test_template_if_guard_rhs_captures_shadowed_outer() {
	v3_bin := figr_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_rhs_capture_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'figr' }\n") or { panic(err) }
	// Guard-bind `item` to a transform OF the outer `item`, then print the guard-local value.
	os.write_file(os.join_path(root, 't.html'), '@if item := transform(item)\n@{item}\n@end\n') or {
		panic(err)
	}
	source := "module main\n\nfn transform(s string) ?string {\n\treturn s + '!'\n}\n\nfn build(item string) string {\n\treturn ('[' + \$tmpl('t.html') + ']').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('outer'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_if_guard_rhs_capture_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	// The RHS read the captured outer `item` ('outer'); the guard bound its transform ('outer!').
	assert run.output.trim_space() == '[outer!]', run.output
}
