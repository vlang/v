import os

const generic_compat_vexe = @VEXE
const generic_compat_tests_dir = os.dir(@FILE)
const generic_compat_v3_dir = os.dir(generic_compat_tests_dir)
const generic_compat_vlib_dir = os.dir(generic_compat_v3_dir)
const generic_compat_v3_source = os.join_path(generic_compat_v3_dir, 'v3.v')

fn generic_compat_tmp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_generic_compat_${name}_${os.getpid()}')
}

fn generic_compat_restore_env(name string, old_value ?string) {
	if value := old_value {
		os.setenv(name, value, true)
	} else {
		os.unsetenv(name)
	}
}

fn test_generic_short_struct_and_alias_method_compile_without_fallback() {
	v3_bin := generic_compat_tmp_path('compiler')
	old_no_fallback := os.getenv_opt('V_MACOS_V3_NO_FALLBACK')
	old_jobs := os.getenv_opt('VJOBS')
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	os.setenv('VJOBS', '1', true)
	defer {
		generic_compat_restore_env('V_MACOS_V3_NO_FALLBACK', old_no_fallback)
		generic_compat_restore_env('VJOBS', old_jobs)
		os.rm(v3_bin) or {}
	}
	build :=
		os.execute('${generic_compat_vexe} -gc none -path "${generic_compat_vlib_dir}|@vlib|@vmodules" -o "${v3_bin}" "${generic_compat_v3_source}"')
	assert build.exit_code == 0, build.output

	fixtures := [
		'generic_fn_short_syntax_struct_param_test.v',
		'generic_fn_type_generic_method_test.v',
	]
	for fixture_name in fixtures {
		fixture := os.join_path(generic_compat_vlib_dir, 'v/tests/generics', fixture_name)
		output := generic_compat_tmp_path(fixture_name.all_before_last('.'))
		defer {
			os.rm(output) or {}
		}
		compile := os.execute('"${v3_bin}" -gc none -no-parallel -o "${output}" "${fixture}"')
		assert compile.exit_code == 0, compile.output
		run := os.execute('"${output}"')
		assert run.exit_code == 0, run.output
	}
}
