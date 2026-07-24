import os

const tmplord_vexe = @VEXE
const tmplord_tests_dir = os.dir(@FILE)
const tmplord_v3_dir = os.dir(tmplord_tests_dir)
const tmplord_vlib_dir = os.dir(tmplord_v3_dir)
const tmplord_v3_src = os.join_path(tmplord_v3_dir, 'v3.v')

fn tmplord_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_subexpr_order_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${tmplord_vexe} -gc none -path "${tmplord_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${tmplord_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A `$tmpl(...)` used as a subexpression must render at its own evaluation position, not
// be hoisted to the front of the statement. `s.bump()` runs first, so the interpolation
// reading `s.n` sees the bumped value (`1`), and a template placed before a side effect
// still renders first (`0`). Hoisting would have rendered every template up front.
fn test_tmpl_subexpr_preserves_evaluation_order() {
	v3_bin := tmplord_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_subexpr_order_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tmplord' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), 'n=@{s.n}') or { panic(err) }
	source := "module main\n\nstruct S {\nmut:\n\tn int\n}\n\nfn (mut s S) bump() string {\n\ts.n++\n\treturn 'B'\n}\n\nfn cat(a string, b string) string {\n\treturn a + '|' + b\n}\n\nfn (mut s S) after() string {\n\treturn cat(s.bump(), \$tmpl('row.txt')).replace('\\n', '')\n}\n\nfn (mut s S) before() string {\n\treturn cat(\$tmpl('row.txt'), s.bump()).replace('\\n', '')\n}\n\nfn main() {\n\tmut a := S{}\n\tprintln(a.after())\n\tmut b := S{}\n\tprintln(b.before())\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_subexpr_order_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().split('\n')
	assert lines.len == 2, run.output
	// bump() ran before the template read s.n
	assert lines[0] == 'B|n=1', run.output
	// a template before the side effect still renders first
	assert lines[1] == 'n=0|B', run.output
}
