import os

const return_only_generic_vexe = @VEXE
const return_only_generic_tests_dir = os.dir(@FILE)
const return_only_generic_v3_dir = os.dir(return_only_generic_tests_dir)
const return_only_generic_vlib_dir = os.dir(return_only_generic_v3_dir)
const return_only_generic_v3_src = os.join_path(return_only_generic_v3_dir, 'v3.v')

fn test_return_only_generic_specializations_emit_bodies() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_return_only_generic_${pid}')
	source := os.join_path(os.temp_dir(), 'v3_return_only_generic_input_${pid}.v')
	output := os.join_path(os.temp_dir(), 'v3_return_only_generic_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(source) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${return_only_generic_vexe} -gc none -d ownership -path "${return_only_generic_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${return_only_generic_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(source, "fn make_default[T]() T {
	return T{}
}

fn err[T](message string) !T {
	return error(message)
}

fn parse_result_special[T](text string) !T {
	if text.len == 0 {
		return err[T]('empty')
	}
	return make_default[T]()
}

fn main() {
	flag := make_default[bool]()
	assert !flag
	value := parse_result_special[int]('') or { 7 }
	assert value == 7
	println('ok')
}
") or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -ownership -d ownership -no-parallel -o ${output} ${source}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(output)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
