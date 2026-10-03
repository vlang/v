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
		app := os.exec([@VEXE, '-new-compiler', '-nocache', '-nocolor',
			...(os.split_args(mode) or { panic(err) }), app_file])
		v := os.exec([@VEXE, '-new-compiler', '-nocache', '-nocolor',
			...(os.split_args(mode) or { panic(err) }), v_file])
		assert app.exit_code != 0, app.output
		assert v.exit_code != 0, v.output
		assert app.output.contains('field `name` of struct `Foo` is immutable'), app.output
		assert app.output.contains('`f` is immutable'), app.output
		assert app.output.replace(os.real_path(app_file), '<input>') == v.output.replace(os.real_path(v_file),
			'<input>'), v.output
		forced := os.exec([@VEXE, '-new-compiler', '-nocache', '-nocolor', '-building-v',
			...(os.split_args(mode) or { panic(err) }), v_file])
		assert forced.exit_code != 0, forced.output
		assert forced.output.contains('field `name` of struct `Foo` is immutable'), forced.output
		assert forced.output.contains('`f` is immutable'), forced.output
		assert !forced.output.contains('C compilation error'), forced.output
	}
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
		result := os.exec([@VEXE, '-new-compiler', '-nocache', '-nocolor',
			...(os.split_args(mode) or { panic(err) }), '-file-list', '${probe}', compiler_source])
		assert result.exit_code != 0, result.output
		assert result.output.contains('`zz_probe` is immutable'), result.output
		assert result.output.contains('cannot assign to `zz_probe`: expected `string`, not `int literal`'), result.output
		assert result.output.contains('`string` has no property `nope`'), result.output
		assert !result.output.contains('C compilation error'), result.output
	}
}

fn test_building_v_reports_reachable_dependency_errors() {
	root := os.join_path(os.vtmp_dir(), 'v3_selfhost_dependency_errors_${os.getpid()}')
	modules := os.join_path(root, 'modules')
	app := os.join_path(root, 'app')
	os.mkdir_all(os.join_path(modules, 'dep'))!
	os.mkdir_all(app)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(modules, 'dep', 'dep.v'), 'module dep\npub fn value() string {\n\tprobe := "abc"\n\tprobe = 5\n\treturn probe.nope\n}\n')!
	entry := os.join_path(app, 'v.v')
	os.write_file(entry, 'import dep\nfn main() { println(dep.value()) }\n')!
	for mode in [['-check'], ['-o', os.join_path(root, 'invalid')]] {
		mut process := os.new_process(@VEXE)
		mut arguments := ['-new-compiler', '-nocache', '-nocolor', '-building-v']
		arguments << mode
		arguments << entry
		process.set_args(arguments)
		mut environment := os.environ()
		environment['VMODULES'] = modules
		process.set_environment(environment)
		process.set_redirect_stdio()
		process.run()
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		assert process.code != 0, output
		process.close()
		assert output.contains('dep.v:') && output.contains('`probe` is immutable'), output
		assert output.contains('cannot assign to `probe`: expected `string`, not `int literal`'), output
		assert output.contains('`string` has no property `nope`'), output
		assert !output.contains('C compilation error'), output
	}
}
