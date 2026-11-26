import os
import v.tempname

// An operator overload body may use the `$tmpl()` compile-time function. Its custom body
// loop must run the same `.veb_template` expansion as normal function/block parsing;
// otherwise the placeholder leaks past the parser (no later phase lowers it) and the
// overload fails to compile.
fn test_operator_body_lowers_veb_template() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_operator_body_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'optmpl' }\n") or { panic(err) }
	os.write_file(os.join_path(root, 'row.txt'), 'sum=@{b.n}') or { panic(err) }
	source := "module main\n\nstruct Box {\n\tn int\n}\n\nfn (b Box) + (o Box) string {\n\treturn \$tmpl('row.txt')\n}\n\nfn main() {\n\tprintln((Box{n: 3} + Box{n: 9}).replace('\\n', ''))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// The template read the left operand's field, so no `.veb_template` leaked.
	assert run.output.trim_space() == 'sum=3', run.output
}
