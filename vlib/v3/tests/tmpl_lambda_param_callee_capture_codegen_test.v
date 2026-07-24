import os

const tlc_vexe = @VEXE
const tlc_tests_dir = os.dir(@FILE)
const tlc_v3_dir = os.dir(tlc_tests_dir)
const tlc_vlib_dir = os.dir(tlc_v3_dir)
const tlc_v3_src = os.join_path(tlc_v3_dir, 'v3.v')

fn tlc_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_lambda_param_callee_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${tlc_vexe} -gc none -path "${tlc_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${tlc_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A `$tmpl()` inside an expression-bodied lambda whose template calls a FUNCTION-VALUED lambda
// parameter — `callbacks.map(|render| $tmpl('row.txt'))` with the template `@{render(row)}` — must
// capture `render` (the lambda parameter) into the inlined IIFE. collect_template_free_idents only
// captures a bare callee when is_local_binding() is true, so the lambda parameters must be
// registered as local bindings while the body is inlined; otherwise the closure omits `render` and
// calls the same-named top-level helper (here returning `TOP:`) instead of the lambda argument, or
// fails to compile. The guard binding shadows a top-level `render`; the parameter must win.
fn test_template_iife_captures_function_valued_lambda_parameter() {
	v3_bin := tlc_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_lambda_param_callee_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tlc' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), '@{render(row)}') or { panic(err) }
	source := "module main\n\nfn render(s string) string {\n\treturn 'TOP:' + s\n}\n\nfn build(row string) string {\n\tcallbacks := [fn (s string) string {\n\t\treturn 'LOCAL:' + s\n\t}]\n\tparts := callbacks.map(|render| \$tmpl('row.txt'))\n\treturn ('<' + parts.join(',') + '>').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('abc'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_lambda_param_callee_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	// The template called the captured lambda parameter (the LOCAL callback), not top-level render.
	assert run.output.trim_space() == '<LOCAL:abc>', run.output
}
