import os

const fixed_enum_vexe = @VEXE
const fixed_enum_tests_dir = os.dir(@FILE)
const fixed_enum_v3_dir = os.dir(fixed_enum_tests_dir)
const fixed_enum_vlib_dir = os.dir(fixed_enum_v3_dir)
const fixed_enum_v3_source = os.join_path(fixed_enum_v3_dir, 'v3.v')

fn fixed_enum_tmp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_fixed_enum_${name}_${os.getpid()}')
}

fn fixed_enum_build_v3() string {
	v3_bin := fixed_enum_tmp_path('compiler')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${fixed_enum_vexe} -gc none -path "${fixed_enum_vlib_dir}|@vlib|@vmodules" -o "${v3_bin}" "${fixed_enum_v3_source}"')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_in_fixed_enum_literal_uses_left_operand_context() {
	v3_bin := fixed_enum_build_v3()
	source_path := fixed_enum_tmp_path('source.v')
	output_path := fixed_enum_tmp_path('output')
	defer {
		os.rm(v3_bin) or {}
		os.rm(source_path) or {}
		os.rm(output_path) or {}
	}
	os.write_file(source_path, 'enum OperationKind {
	eq
	in
	not_in
}

fn is_array_operation(kind OperationKind) bool {
	return kind in [.in, .not_in]!
}

fn main() {
	assert is_array_operation(.in)
	assert is_array_operation(.not_in)
	assert !is_array_operation(.eq)
}
') or {
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
}
