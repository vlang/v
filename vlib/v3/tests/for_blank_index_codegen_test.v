import os

const for_blank_index_vexe = @VEXE
const for_blank_index_tests_dir = os.dir(@FILE)
const for_blank_index_v3_dir = os.dir(for_blank_index_tests_dir)
const for_blank_index_vlib_dir = os.dir(for_blank_index_v3_dir)
const for_blank_index_v3_src = os.join_path(for_blank_index_v3_dir, 'v3.v')

fn for_blank_index_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_for_blank_index_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${for_blank_index_vexe} -gc none -path "${for_blank_index_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${for_blank_index_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_for_in_blank_index_uses_synthetic_c_loop_index() {
	v3_bin := for_blank_index_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_blank_index_input_${os.getpid()}.v')
	os.write_file(src, 'fn main() {
	arr := [1, 2, 3]
	mut sum := 0
	for _, value in arr {
		sum += value
	}
	assert sum == 6
	println("ok")
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_for_blank_index_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_code := os.read_file(bin + '.c') or { panic(err) }
	assert !c_code.contains('for (; _ < arr.len; _++)'), c_code
	assert !c_code.contains('array_get(arr, _)'), c_code
	assert c_code.contains('__for_idx_') || c_code.contains('__discard_'), c_code

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
