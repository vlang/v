import os

const top_vexe = @VEXE
const top_tests_dir = os.dir(@FILE)
const top_v3_dir = os.dir(top_tests_dir)
const top_vlib_dir = os.dir(top_v3_dir)
const top_v3_src = os.join_path(top_v3_dir, 'v3.v')

fn top_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_operator_param_capture_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${top_vexe} -gc none -path "${top_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${top_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// An operator-overload body contains a `$tmpl()` (a subexpression template, lowered to an IIFE)
// whose interpolation calls a function-valued OPERATOR PARAMETER — `@{render(r.row)}` where the
// operator takes `render fn (string) string`. The operator body's local-binding scope must be
// seeded with its parameters (as fn_decl_body does), or collect_template_free_idents treats the
// bare callee `render` as a top-level helper and omits the capture, so the closure calls the wrong
// symbol (the top-level `render`, returning `TOP:`) instead of the operator argument (`LOCAL:`).
fn test_operator_body_template_captures_function_valued_parameter() {
	v3_bin := top_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_operator_param_capture_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'top' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), '@{render(r.row)}') or { panic(err) }
	source := "module main\n\nfn render(s string) string {\n\treturn 'TOP:' + s\n}\n\nstruct R {\n\trow string\n}\n\nfn (r R) + (render fn (string) string) string {\n\treturn ('<' + \$tmpl('row.txt') + '>').replace('\\n', '')\n}\n\nfn main() {\n\tr := R{\n\t\trow: 'abc'\n\t}\n\tcb := fn (s string) string {\n\t\treturn 'LOCAL:' + s\n\t}\n\tprintln(r + cb)\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_operator_param_capture_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	// The template called the captured operator parameter (LOCAL:), not the top-level render.
	assert run.output.trim_space() == '<LOCAL:abc>', run.output
}
