import os

const optmpl_vexe = @VEXE
const optmpl_tests_dir = os.dir(@FILE)
const optmpl_v3_dir = os.dir(optmpl_tests_dir)
const optmpl_vlib_dir = os.dir(optmpl_v3_dir)
const optmpl_v3_src = os.join_path(optmpl_v3_dir, 'v3.v')

fn optmpl_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_operator_body_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${optmpl_vexe} -gc none -path "${optmpl_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${optmpl_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// An operator overload body may use the `$tmpl()` compile-time function. Its custom body
// loop must run the same `.veb_template` expansion as normal function/block parsing;
// otherwise the placeholder leaks past the parser (no later phase lowers it) and the
// overload fails to compile.
fn test_operator_body_lowers_veb_template() {
	v3_bin := optmpl_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_operator_body_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'optmpl' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), 'sum=@{b.n}') or { panic(err) }
	source := "module main\n\nstruct Box {\n\tn int\n}\n\nfn (b Box) + (o Box) string {\n\treturn \$tmpl('row.txt')\n}\n\nfn main() {\n\tprintln((Box{n: 3} + Box{n: 9}).replace('\\n', ''))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_operator_body_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	// The template read the left operand's field, so no `.veb_template` leaked.
	assert run.output.trim_space() == 'sum=3', run.output
}
