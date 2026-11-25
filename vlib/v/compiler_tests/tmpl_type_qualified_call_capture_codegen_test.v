import os
import v.tempname

// A template lowered into an IIFE (here because `$tmpl()` is a subexpression of `+`) that calls
// a type-qualified static helper `@{Tool.make(row)}` must capture only the real local (`row`),
// not the TYPE name `Tool`. The callee is a selector, so the capture collector descends into its
// base; without excluding type/static names it would add `Tool` to the closure's capture list
// and emit an invalid `fn [Tool]` capture, breaking compilation.
fn test_tmpl_iife_does_not_capture_type_qualified_callee_base() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_type_qualified_call_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tqcc' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.html'), '@{Tool.make(row)}') or { panic(err) }
	source := "module main\n\nstruct Tool {\n\tname string\n}\n\nfn Tool.make(s string) string {\n\treturn 'MADE:' + s\n}\n\nfn build(row string) string {\n\treturn '[' + \$tmpl('row.html') + ']'\n}\n\nfn main() {\n\tprintln(build('abc').replace('\\n', ''))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// The static helper ran with the captured local `row`; the type name was not captured.
	assert run.output.trim_space() == '[MADE:abc]', run.output
}
