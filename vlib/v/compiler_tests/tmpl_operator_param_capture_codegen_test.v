import os
import v.tempname

// An operator-overload body contains a `$tmpl()` (a subexpression template, lowered to an IIFE)
// whose interpolation calls a function-valued OPERATOR PARAMETER — `@{render(row)}` where the
// operator on `type Render = fn (string) string` takes `render Render`. The operator body's
// local-binding scope must be seeded with its parameters (as fn_decl_body does), or
// collect_template_free_idents treats the bare callee `render` as a top-level helper and omits
// the capture, so the closure calls the wrong symbol (the top-level `render`, returning `TOP:`)
// instead of the operator argument (`LOCAL:`).
fn test_operator_body_template_captures_function_valued_parameter() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_operator_param_capture_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'top' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), '@{render(row)}') or { panic(err) }
	source := "module main\n\ntype Render = fn (string) string\n\nfn render(s string) string {\n\treturn 'TOP:' + s\n}\n\nfn (prefix Render) + (render Render) Render {\n\trow := prefix('abc')\n\ttext := \$tmpl('row.txt').replace('\\n', '')\n\treturn fn [text] (s string) string {\n\t\treturn '<' + text + s + '>'\n\t}\n}\n\nfn main() {\n\tprefix := Render(fn (s string) string {\n\t\treturn 'P:' + s\n\t})\n\tcb := Render(fn (s string) string {\n\t\treturn 'LOCAL:' + s\n\t})\n\tcombined := prefix + cb\n\tprintln(combined('!'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// The template called the captured operator parameter (LOCAL:), not the top-level render.
	assert run.output.trim_space() == '<LOCAL:P:abc!>', run.output
}

// Both operands of an overloaded operator have the same type (doc/docs.md, "Limited operator
// overloading"). A function-typed operand used to be accepted in the declaration, although
// every use of the operator was then rejected.
fn test_operator_with_function_typed_operand_is_rejected() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_operator_fn_operand_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_tmpl_operator_fn_operand.v')
	os.write_file(source, "struct R {\n\trow string\n}\n\nfn (r R) + (render fn (string) string) string {\n\treturn render(r.row)\n}\n\nfn main() {\n\tprintln(R{row: 'abc'}.row)\n}\n") or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-check', source])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('expected `R` not `fn(string) string` - both operands must be the same type for operator overloading'), compile.output
}
