import os

const local_fixed_array_vexe = @VEXE
const local_fixed_array_tests_dir = os.dir(@FILE)
const local_fixed_array_v3_dir = os.dir(local_fixed_array_tests_dir)
const local_fixed_array_vlib_dir = os.dir(local_fixed_array_v3_dir)
const local_fixed_array_v3_src = os.join_path(local_fixed_array_v3_dir, 'v3.v')

fn local_fixed_array_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fixed_array_local_zero_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${local_fixed_array_vexe} -gc none -path "${local_fixed_array_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${local_fixed_array_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_local_fixed_array_zero_init_declarations_use_direct_c_arrays() {
	v3_bin := local_fixed_array_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_fixed_array_local_zero_${os.getpid()}.v')
	os.write_file(src, 'module main

type Handle = voidptr

fn local_score() int {
	mut direct := [4]int{}
	direct[0] = 1
	mut wrapped := unsafe { [4]int{} }
	wrapped[1] = 2
	mut handles := unsafe { [32]Handle{} }
	handles[0] = voidptr(0)
	mut nested := unsafe { [2][3]int{} }
	nested[1][2] = 3
	handle_score := if handles[0] == voidptr(0) { 4 } else { 0 }
	return direct[0] + wrapped[1] + nested[1][2] + handle_score
}

fn main() {
	println(int_str(local_score()))
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_fixed_array_local_zero_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '10'
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('int direct[4] = {0};'), generated
	assert generated.contains('int wrapped[4] = {0};'), generated
	assert generated.contains('handles[32] = {0};'), generated
	assert generated.contains('int nested[2][3] = {0};'), generated
	assert !generated.contains(' = (Array_fixed_'), generated
	assert !generated.contains('Array_fixed_voidptr_32 handles'), generated
}
