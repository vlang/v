import os

const generic_sum_str_vexe = @VEXE
const generic_sum_str_tests_dir = os.dir(@FILE)
const generic_sum_str_v3_dir = os.dir(generic_sum_str_tests_dir)
const generic_sum_str_vlib_dir = os.dir(generic_sum_str_v3_dir)
const generic_sum_str_v3_src = os.join_path(generic_sum_str_v3_dir, 'v3.v')

fn generic_sum_str_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_sum_str_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${generic_sum_str_vexe} -gc none -path "${generic_sum_str_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${generic_sum_str_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_generic_sum_str_is_used_for_string_interpolation() {
	v3_bin := generic_sum_str_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_generic_sum_str_input_${os.getpid()}.v')
	os.write_file(src, "type Maybe[T] = Empty | Some[T]

struct Empty {}

struct Some[T] {
	value T
}

fn (m Maybe[T]) str() string {
	return 'maybe'
}

fn main() {
	x := Maybe[int](Some[int]{
		value: 7
	})
	interpolated := 'value \${x}'
	assert interpolated == 'value maybe'
	println(interpolated)
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_generic_sum_str_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'value maybe'

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('Maybe_int__str'), generated
	assert !generated.contains(' = x;'), generated
	assert !generated.contains('Maybe_T__str'), generated
}
