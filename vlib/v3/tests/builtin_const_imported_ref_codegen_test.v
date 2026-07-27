import os

const builtin_const_imported_vexe = @VEXE
const builtin_const_imported_tests_dir = os.dir(@FILE)
const builtin_const_imported_v3_dir = os.dir(builtin_const_imported_tests_dir)
const builtin_const_imported_vlib_dir = os.dir(builtin_const_imported_v3_dir)
const builtin_const_imported_v3_src = os.join_path(builtin_const_imported_v3_dir, 'v3.v')

fn builtin_const_imported_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_builtin_const_imported_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${builtin_const_imported_vexe} -gc none -path "${builtin_const_imported_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${builtin_const_imported_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_builtin_const_ref_from_imported_module_is_qualified() {
	v3_bin := builtin_const_imported_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_builtin_const_imported_input_${os.getpid()}.v')
	os.write_file(src, "import strconv

fn main() {
	n, err := strconv.common_parse_uint2('18446744073709551615', 10, 64)
	assert err == 0
	assert n == max_u64
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_builtin_const_imported_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	// This test inspects the imported implementation in the monolithic C output.
	compile := os.execute('${v3_bin} -nocache ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert c_code.contains('#define builtin__max_u64'), c_code
	assert c_code.contains('builtin__max_u64 / (u64)(base)'), c_code
	assert c_code.contains('__if_val_') && c_code.contains('builtin__max_u64;'), c_code
	assert !c_code.contains('(max_u64 /'), c_code
	assert !c_code.contains('= max_u64;'), c_code
}
