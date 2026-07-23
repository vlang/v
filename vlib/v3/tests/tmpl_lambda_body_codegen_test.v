import os

const tlb_vexe = @VEXE
const tlb_tests_dir = os.dir(@FILE)
const tlb_v3_dir = os.dir(tlb_tests_dir)
const tlb_vlib_dir = os.dir(tlb_v3_dir)
const tlb_v3_src = os.join_path(tlb_v3_dir, 'v3.v')

fn tlb_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_lambda_body_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${tlb_vexe} -gc none -path "${tlb_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${tlb_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A `$tmpl()` used in an expression-bodied lambda must be lowered in place. A lambda body is
// parsed as a bare expression and is never re-expanded via parse_block_body, so the
// `.veb_template` placeholder would otherwise leak past the parser (no later phase handles it).
// Exercises both reviewer forms: a `|r| $tmpl(...)` passed to `.map` (whose template references the
// lambda parameter `r`, so the inlined IIFE must capture it) and a no-arg `|| $tmpl(...)` bound to a
// variable and called later.
fn test_template_in_expression_lambda_bodies_are_lowered() {
	v3_bin := tlb_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_lambda_body_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tlb' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), '<@{r}>') or { panic(err) }
	os.write_file(os.join_path(root, 'greet.txt'), 'hi-there') or { panic(err) }
	source := "module main\n\nfn build_map() string {\n\trows := ['a', 'b', 'c']\n\tparts := rows.map(|r| \$tmpl('row.txt'))\n\treturn parts.join(',').replace('\\n', '')\n}\n\nfn build_noarg() string {\n\tcb := || \$tmpl('greet.txt')\n\treturn cb().replace('\\n', '')\n}\n\nfn main() {\n\tprintln(build_map())\n\tprintln(build_noarg())\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_lambda_body_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().split('\n')
	assert lines.len == 2, run.output
	// The `.map` lambda rendered the template once per element, capturing the loop value `r`.
	assert lines[0] == '<a>,<b>,<c>', run.output
	// The no-arg lambda's template rendered when the stored closure was called.
	assert lines[1] == 'hi-there', run.output
}
