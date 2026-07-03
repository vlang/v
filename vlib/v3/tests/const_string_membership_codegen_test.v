import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3_const_string_membership() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_const_string_membership_codegen_test_${os.getpid()}')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_const_string_array_membership_compiles_on_c_backend() {
	v3_bin := build_v3_const_string_membership()
	src_path := os.join_path(os.temp_dir(), 'v3_const_string_membership_${os.getpid()}.v')
	os.write_file(src_path,
		"const names = ['malloc', 'free']\n\nfn has_name(name string) bool {\n\treturn name in names\n}\n\nfn main() {\n\tprintln(has_name('malloc'))\n\tprintln(has_name('memcpy'))\n}\n") or {
		panic(err)
	}
	bin_path := os.join_path(os.temp_dir(), 'v3_const_string_membership_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${bin_path}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true\nfalse'
}
