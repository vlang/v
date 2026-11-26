import os
import v.tempname

// test_union_aliasing_and_byte_array_contains_codegen validates this v3 regression case.
fn test_union_aliasing_and_byte_array_contains_codegen() {
	dir := os.join_path(os.vtmp_dir(), 'v3_union_byte_contains_codegen_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'main.v')
	os.write_file(src,
		"union Bits {\nmut:\n\tf f64\n\tu u64\n}\n\nfn main() {\n\tmut u := Bits{}\n\tu.f = 12.5\n\tif u.u != u64(0) {\n\t\tprintln('union ok')\n\t} else {\n\t\tprintln('union bad')\n\t}\n\tmut bytes := []u8{len: 4, init: 0}\n\tbytes[1] = `.`\n\tif `.` in bytes {\n\t\tprintln('byte contains ok')\n\t} else {\n\t\tprintln('byte contains bad')\n\t}\n\tprintln(12.5.str())\n}\n")!

	bin := os.join_path(dir, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'union ok\nbyte contains ok\n12.5'
}
