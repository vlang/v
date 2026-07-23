import os

const dls_vexe = @VEXE
const dls_tests_dir = os.dir(@FILE)
const dls_v3_dir = os.dir(dls_tests_dir)
const dls_vlib_dir = os.dir(dls_v3_dir)
const dls_v3_src = os.join_path(dls_v3_dir, 'v3.v')

fn dls_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_decl_lhs_shadow_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${dls_vexe} -gc none -path "${dls_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${dls_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A declaration whose RHS is a subexpression template that calls a same-named top-level helper —
// `render := ('<' + $tmpl('row.txt') + '>')` with the template `@{render(row)}` — must treat
// `render` inside the template as the TOP-LEVEL helper: the local `render` is not in scope during
// its own initializer, so the inlined IIFE must not capture it. Otherwise the closure captures a
// variable that does not exist yet and breaks valid shadowing.
fn test_template_decl_lhs_not_captured_during_its_own_rhs() {
	v3_bin := dls_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_decl_lhs_shadow_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'dls' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), '@{render(row)}') or { panic(err) }
	source := "module main\n\nfn render(s string) string {\n\treturn 'TOP:' + s\n}\n\nfn build(row string) string {\n\trender := ('<' + \$tmpl('row.txt') + '>').replace('\\n', '')\n\treturn render\n}\n\nfn main() {\n\tprintln(build('abc'))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_decl_lhs_shadow_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	// The template called the top-level `render` (TOP:), not the local being declared.
	assert run.output.trim_space() == '<TOP:abc>', run.output
}
