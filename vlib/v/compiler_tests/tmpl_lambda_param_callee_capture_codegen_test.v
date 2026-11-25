import os
import v.tempname

// A `$tmpl()` inside an expression-bodied lambda whose template calls a FUNCTION-VALUED lambda
// parameter — `callbacks.map(|render| $tmpl('row.txt'))` with the template `@{render(row)}` — must
// capture `render` (the lambda parameter) into the inlined IIFE. collect_template_free_idents only
// captures a bare callee when is_local_binding() is true, so the lambda parameters must be
// registered as local bindings while the body is inlined; otherwise the closure omits `render` and
// calls the same-named top-level helper (here returning `TOP:`) instead of the lambda argument, or
// fails to compile. The guard binding shadows a top-level `render`; the parameter must win.
fn test_template_iife_captures_function_valued_lambda_parameter() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_lambda_param_callee_capture_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tlc' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), '@{render(row)}') or { panic(err) }
	source := "module main\n\nfn render(s string) string {\n\treturn 'TOP:' + s\n}\n\nfn build(row string) string {\n\tcallbacks := [fn (s string) string {\n\t\treturn 'LOCAL:' + s\n\t}]\n\tparts := callbacks.map(|render| \$tmpl('row.txt'))\n\treturn ('<' + parts.join(',') + '>').replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build('abc'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// The template called the captured lambda parameter (the LOCAL callback), not top-level render.
	assert run.output.trim_space() == '<LOCAL:abc>', run.output
}
