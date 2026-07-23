import os

const tqcc_vexe = @VEXE
const tqcc_tests_dir = os.dir(@FILE)
const tqcc_v3_dir = os.dir(tqcc_tests_dir)
const tqcc_vlib_dir = os.dir(tqcc_v3_dir)
const tqcc_v3_src = os.join_path(tqcc_v3_dir, 'v3.v')

fn tqcc_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_type_qualified_call_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${tqcc_vexe} -gc none -path "${tqcc_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${tqcc_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) that calls
// a type-qualified static helper `@{Tool.make(row)}` must capture only the real local (`row`),
// not the TYPE name `Tool`. The callee is a selector, so the capture collector descends into its
// base; without excluding type/static names it would add `Tool` to the closure's capture list
// and emit an invalid `fn [Tool]` capture, breaking compilation.
fn test_tmpl_iife_does_not_capture_type_qualified_callee_base() {
	v3_bin := tqcc_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_type_qualified_call_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tqcc' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.html'), '@{Tool.make(row)}') or { panic(err) }
	source := "module main\n\nstruct Tool {\n\tname string\n}\n\nfn Tool.make(s string) string {\n\treturn 'MADE:' + s\n}\n\nfn build(row string) string {\n\treturn '[' + \$tmpl('row.html') + ']'\n}\n\nfn main() {\n\tprintln(build('abc').replace('\\n', ''))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_type_qualified_call_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	// The static helper ran with the captured local `row`; the type name was not captured.
	assert run.output.trim_space() == '[MADE:abc]', run.output
}
