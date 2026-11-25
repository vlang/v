import os
import v.tempname

fn test_const_string_array_membership_compiles_on_c_backend() {
	dir := os.join_path(os.vtmp_dir(), 'v3_const_string_membership_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	src_path := os.join_path(dir, 'main.v')
	os.write_file(src_path,
		"const names = ['malloc', 'free']\n\nfn has_name(name string) bool {\n\treturn name in names\n}\n\nfn main() {\n\tprintln(has_name('malloc'))\n\tprintln(has_name('memcpy'))\n}\n") or {
		panic(err)
	}
	bin_path := os.join_path(dir, 'app')
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true\nfalse'
}
