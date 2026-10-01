module driver

import os

fn test_building_v_detection_requires_compiler_entry_paths() {
	assert input_implies_building_v(os.join_path(@VEXEROOT, 'cmd', 'v'))
	assert input_implies_building_v(os.join_path(@VEXEROOT, 'cmd', 'v', 'v.v'))
	assert input_implies_building_v(os.join_path(@VEXEROOT, 'vlib', 'v'))
	assert input_implies_building_v(os.join_path(@VEXEROOT, 'vlib', 'v', 'v.v'))
	assert !input_implies_building_v('v.v')
	assert !input_implies_building_v(os.join_path(os.vtmp_dir(), 'ordinary_program', 'v.v'))
}

fn test_ordinary_v_v_file_has_the_same_checker_errors_as_app_v() {
	dir := os.join_path(os.vtmp_dir(), 'v3_building_v_name_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := "struct Foo {\n\tname string\n}\nfn rename(f Foo) Foo {\n\tf.name = 'renamed'\n\treturn f\n}\nfn main() {\n\tprintln(rename(Foo{'a'}).name)\n}\n"
	app_file := os.join_path(dir, 'app.v')
	v_file := os.join_path(dir, 'v.v')
	os.write_file(app_file, source)!
	os.write_file(v_file, source)!
	compiler := os.quoted_path(@VEXE)
	for mode in ['-check', '-o ${os.quoted_path(os.join_path(dir, 'invalid'))}'] {
		app := os.execute('${compiler} -new-compiler -nocache -nocolor ${mode} ${os.quoted_path(app_file)}')
		v := os.execute('${compiler} -new-compiler -nocache -nocolor ${mode} ${os.quoted_path(v_file)}')
		assert app.exit_code != 0, app.output
		assert v.exit_code != 0, v.output
		assert app.output.contains('field `name` of struct `Foo` is immutable'), app.output
		assert app.output.contains('`f` is immutable'), app.output
		assert app.output.replace(os.real_path(app_file), '<input>') == v.output.replace(os.real_path(v_file),
			'<input>'), v.output
	}
	forced := os.execute('${compiler} -new-compiler -nocache -nocolor -building-v -check ${os.quoted_path(v_file)}')
	assert forced.exit_code != 0, forced.output
	assert forced.output.contains('field `name` of struct `Foo` is immutable'), forced.output
	assert forced.output.contains('`f` is immutable'), forced.output
}

fn test_cmd_v_build_reports_errors_from_an_isolated_source_file() {
	dir := os.join_path(os.vtmp_dir(), 'v3_building_v_diagnostics_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	probe := os.join_path(dir, 'compiler_probe.v')
	os.write_file(probe, "fn selfhost_diagnostic_probe() {\n\tzz_probe := 'abc'\n\tzz_probe = 5\n\teprintln(zz_probe.nope)\n}\n")!
	compiler_source := os.join_path(@VEXEROOT, 'cmd', 'v')
	compiler := os.quoted_path(@VEXE)
	for mode in ['-check', '-o ${os.quoted_path(os.join_path(dir, 'invalid_compiler'))}'] {
		result := os.execute('${compiler} -new-compiler -nocache -nocolor ${mode} -file-list ${os.quoted_path(probe)} ${os.quoted_path(compiler_source)}')
		assert result.exit_code != 0, result.output
		assert result.output.contains('`zz_probe` is immutable'), result.output
		assert result.output.contains('cannot assign to `zz_probe`: expected `string`, not `int literal`'), result.output
		assert result.output.contains('`string` has no property `nope`'), result.output
		assert !result.output.contains('C compilation error'), result.output
	}
}
