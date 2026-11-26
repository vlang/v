import os
import v.tempname

fn cfor_render_project(name string, main_src string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.html'), '@{render(row)}') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), main_src) or { panic(err) }
	bin := os.join_path(root, 'app')
	// -nocache: both subtests build a project with an anonymous fn returned from get_render;
	// sharing the module cache across them collides their `anon_fn` symbols in a cached object
	// (a pre-existing cache quirk unrelated to capture collection).
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-nocache', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A `$tmpl()` used as a subexpression of `+` is lowered to a nested immediately-invoked
// closure; v3 closures do not auto-capture, so the closure must explicitly capture the locals
// the template references. When the template `@{render(row)}` sits in the body of a C-style
// loop whose init declares the callee (`for render := get_render(); ...`), the loop-init
// binding must be registered as an in-scope local so the callee is captured — otherwise it is
// misclassified as a top-level helper and the generated closure references an undefined
// `render`.
fn test_c_style_for_init_binding_is_captured() {
	src := "module main\n\nfn get_render() fn (string) string {\n\treturn fn (s string) string {\n\t\treturn 'R:' + s\n\t}\n}\n\nfn build(row string) string {\n\tmut result := ''\n\tmut i := 0\n\tfor render := get_render(); i < 1; i++ {\n\t\tresult = ('[' + \$tmpl('row.html') + ']').replace('\\n', '')\n\t}\n\treturn result\n}\n\nfn main() {\n\tprintln(build('abc'))\n}\n"
	out := cfor_render_project('tmpl_c_style_for_init_capture', src)
	assert out == '[R:abc]'
}

// The same must hold for a multi-init C-style loop (`for i, render := 0, get_render(); ...`),
// where each `:=` binding is in scope for the body.
fn test_c_style_for_multi_init_binding_is_captured() {
	src := "module main\n\nfn get_render() fn (string) string {\n\treturn fn (s string) string {\n\t\treturn 'R:' + s\n\t}\n}\n\nfn build(row string) string {\n\tmut result := ''\n\tfor i, render := 0, get_render(); i < 1; i++ {\n\t\tresult = ('[' + \$tmpl('row.html') + ']').replace('\\n', '')\n\t}\n\treturn result\n}\n\nfn main() {\n\tprintln(build('abc'))\n}\n"
	out := cfor_render_project('tmpl_c_style_for_multi_init_capture', src)
	assert out == '[R:abc]'
}
