import os

const top_vexe = @VEXE
const top_tests_dir = os.dir(@FILE)
const top_v3_dir = os.dir(top_tests_dir)
const top_vlib_dir = os.dir(top_v3_dir)
const top_v3_src = os.join_path(top_v3_dir, 'v.v')
const top_v3_bin = os.join_path(os.temp_dir(), 'v3_tmpl_operator_param_capture_test_${os.getpid()}')

fn testsuite_begin() {
	os.rm(top_v3_bin) or {}
	build :=
		os.exec([top_vexe, '-gc', 'none', '-path', '${top_vlib_dir}' + '|@vlib|@vmodules', '-o',
			top_v3_bin, '${top_v3_src}'])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rm(top_v3_bin) or {}
}

// An operator-overload body contains a `$tmpl()` (a subexpression template, lowered to an IIFE)
// whose interpolation calls a function-valued OPERATOR PARAMETER — `@{render(row)}` where the
// operator on `type Render = fn (string) string` takes `render Render`. The operator body's
// local-binding scope must be seeded with its parameters (as fn_decl_body does), or
// collect_template_free_idents treats the bare callee `render` as a top-level helper and omits
// the capture, so the closure calls the wrong symbol (the top-level `render`, returning `TOP:`)
// instead of the operator argument (`LOCAL:`).
fn test_operator_body_template_captures_function_valued_parameter() {
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_operator_param_capture_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'top' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), '@{render(row)}') or { panic(err) }
	source := "module main\n\ntype Render = fn (string) string\n\nfn render(s string) string {\n\treturn 'TOP:' + s\n}\n\nfn (prefix Render) + (render Render) Render {\n\trow := prefix('abc')\n\ttext := \$tmpl('row.txt').replace('\\n', '')\n\treturn fn [text] (s string) string {\n\t\treturn '<' + text + s + '>'\n\t}\n}\n\nfn main() {\n\tprefix := Render(fn (s string) string {\n\t\treturn 'P:' + s\n\t})\n\tcb := Render(fn (s string) string {\n\t\treturn 'LOCAL:' + s\n\t})\n\tcombined := prefix + cb\n\tprintln(combined('!'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_operator_param_capture_bin_${pid}')
	defer {
		os.rm(bin) or {}
	}
	compile := os.exec([top_v3_bin, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
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
	pid := os.getpid()
	source := os.join_path(os.temp_dir(), 'v3_tmpl_operator_fn_operand_${pid}.v')
	defer {
		os.rm(source) or {}
	}
	os.write_file(source, "struct R {\n\trow string\n}\n\nfn (r R) + (render fn (string) string) string {\n\treturn render(r.row)\n}\n\nfn main() {\n\tprintln(R{row: 'abc'}.row)\n}\n") or {
		panic(err)
	}
	compile := os.exec([top_v3_bin, '-check', source])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('expected `R` not `fn(string) string` - both operands must be the same type for operator overloading'), compile.output
}
