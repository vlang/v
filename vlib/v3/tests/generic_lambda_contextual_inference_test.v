import os

const generic_lambda_vexe = @VEXE
const generic_lambda_tests_dir = os.dir(@FILE)
const generic_lambda_v3_dir = os.dir(generic_lambda_tests_dir)
const generic_lambda_vlib_dir = os.dir(generic_lambda_v3_dir)
const generic_lambda_v3_source = os.join_path(generic_lambda_v3_dir, 'v3.v')

fn generic_lambda_tmp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_generic_lambda_${name}_${os.getpid()}')
}

fn generic_lambda_build_v3() string {
	v3_bin := generic_lambda_tmp_path('compiler')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${generic_lambda_vexe} -gc none -path "${generic_lambda_vlib_dir}|@vlib|@vmodules" -o "${v3_bin}" "${generic_lambda_v3_source}"')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_generic_lambda_infers_return_from_contextual_parameter() {
	v3_bin := generic_lambda_build_v3()
	source_path := generic_lambda_tmp_path('source.v')
	output_path := generic_lambda_tmp_path('output')
	defer {
		os.rm(v3_bin) or {}
		os.rm(source_path) or {}
		os.rm(output_path) or {}
	}
	os.write_file(source_path, "struct Box[T] {
	value T
}

fn (value Box[T]) map[U](op fn (T) U) Box[U] {
	return Box[U]{
		value: op(value.value)
	}
}

fn apply[T, U](value T, op fn (T) U) U {
	return op(value)
}

fn main() {
	mapped := Box[string]{value: 'hello'}.map(|s| s.len)
	applied := apply('world', |s| s.len)
	println(mapped.value)
	println(applied)
}
") or {
		panic(err)
	}
	old_no_fallback := os.getenv_opt('V_MACOS_V3_NO_FALLBACK')
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	compile := os.execute('"${v3_bin}" -o "${output_path}" "${source_path}"')
	if value := old_no_fallback {
		os.setenv('V_MACOS_V3_NO_FALLBACK', value, true)
	} else {
		os.unsetenv('V_MACOS_V3_NO_FALLBACK')
	}
	assert compile.exit_code == 0, compile.output
	run := os.execute('"${output_path}"')
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '5\n5'
}
