import os
import v.tempname

// A declaration whose RHS is a subexpression template that calls a same-named top-level helper —
// `render := ('<' + $tmpl('row.txt') + '>')` with the template `@{render(row)}` — must treat
// `render` inside the template as the TOP-LEVEL helper: the local `render` is not in scope during
// its own initializer, so the inlined IIFE must not capture it. Otherwise the closure captures a
// variable that does not exist yet and breaks valid shadowing.
fn test_template_decl_lhs_not_captured_during_its_own_rhs() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_decl_lhs_shadow_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'dls' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), '@{render(row)}') or { panic(err) }
	source := "module main\n\nfn render(s string) string {\n\treturn 'TOP:' + s\n}\n\nfn build(row string) string {\n\trender := ('<' + \$tmpl('row.txt') + '>').replace('\\n', '')\n\treturn render\n}\n\nfn main() {\n\tprintln(build('abc'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// The template called the top-level `render` (TOP:), not the local being declared.
	assert run.output.trim_space() == '<TOP:abc>', run.output
}
