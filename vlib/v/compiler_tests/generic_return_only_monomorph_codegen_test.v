// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_return_only_generic_specializations_emit_bodies() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_return_only_generic_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_return_only_generic_input.v')
	output := os.join_path(root, 'v3_return_only_generic_program')
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
	compile := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-no-parallel',
		'-o', output, source], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert compile.exit_code == 0, compile.output
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
