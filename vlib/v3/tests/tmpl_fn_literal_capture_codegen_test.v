import os

const capfn_vexe = @VEXE
const capfn_tests_dir = os.dir(@FILE)
const capfn_v3_dir = os.dir(capfn_tests_dir)
const capfn_vlib_dir = os.dir(capfn_v3_dir)
const capfn_v3_src = os.join_path(capfn_v3_dir, 'v3.v')

fn capfn_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_fn_literal_capture_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${capfn_vexe} -gc none -path "${capfn_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${capfn_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A `$tmpl()` subexpression inside a closure `fn [render] (...) { ... }` is lowered to a
// nested immediately-invoked closure. When the template calls a captured function
// (`@{render(row)}`), that callee is an in-scope local binding of the closure body, so the
// nested IIFE must capture it — v3 closures do not auto-capture. Here the captured `render`
// shadows a top-level `render`; the captured local must win (`LOCAL:`), proving the closure
// capture list is registered for the template IIFE.
fn test_tmpl_iife_captures_fn_literal_capture() {
	v3_bin := capfn_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_fn_literal_capture_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'capfn' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.html'), '@{render(row)}') or { panic(err) }
	source := "module main\n\nfn render(s string) string {\n\treturn 'TOP:' + s\n}\n\nfn build(arg string) string {\n\trender := fn (s string) string {\n\t\treturn 'LOCAL:' + s\n\t}\n\tf := fn [render] (row string) string {\n\t\treturn '[' + \$tmpl('row.html') + ']'\n\t}\n\treturn f(arg).replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('abc'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_fn_literal_capture_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '[LOCAL:abc]', run.output
}
