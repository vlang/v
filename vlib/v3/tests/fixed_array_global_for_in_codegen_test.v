import os

const fixed_array_global_for_vexe = @VEXE
const fixed_array_global_for_tests_dir = os.dir(@FILE)
const fixed_array_global_for_v3_dir = os.dir(fixed_array_global_for_tests_dir)
const fixed_array_global_for_vlib_dir = os.dir(fixed_array_global_for_v3_dir)
const fixed_array_global_for_v3_src = os.join_path(fixed_array_global_for_v3_dir, 'v3.v')

fn fixed_array_global_for_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_fixed_array_global_for_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${fixed_array_global_for_vexe} -gc none -path "${fixed_array_global_for_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${fixed_array_global_for_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_fixed_array_global_for_in_uses_fixed_array_indexing() {
	v3_bin := fixed_array_global_for_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_fixed_array_global_for_${os.getpid()}.v')
	os.write_file(src, 'module main

__global gnums = [6, 7, 8]!

fn sum_gnums() int {
	mut total := 0
	for n in gnums {
		total += n
	}
	return total
}

fn main() {
	println(sum_gnums().str())
}
') or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_fixed_array_global_for_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '21', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	compact := generated.replace('\t', '').replace(' ', '').replace('\n', '')
	assert compact.contains('intgnums[3]'), generated
	assert compact.contains('memmove(gnums,'), generated
	assert compact.contains('=gnums['), generated
	assert !compact.contains('array_get(gnums,'), generated
}
