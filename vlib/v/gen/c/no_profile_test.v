module c

import os

fn test_no_profile_resets_previous_function_instrumentation() {
	mut g := FlatGen.new()
	g.set_profile('-', false, ['main__selected'])
	g.gen_profile_fn_begin('main__selected', 'main', 'selected', false, false)
	assert g.profile_fn_active
	assert g.profile_fn_restore_enabled
	before := g.sb.len
	g.gen_profile_fn_begin('main__ignored', 'main', 'ignored', false, true)
	g.gen_profile_fn_exit()
	assert !g.profile_fn_active
	assert !g.profile_fn_restore_enabled
	assert g.sb.len == before
	g.gen_profile_fn_begin('main__after', 'main', 'after', false, false)
	assert g.profile_fn_active
	assert !g.profile_fn_restore_enabled
	assert g.profile_counters.map(it.fn_name) == ['main__selected', 'main__after']
}

fn test_no_profile_attribute_omits_only_annotated_functions() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'no_profile_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(os.join_path(@VEXEROOT, 'vlib/v/gen/c/testdata/no_profile.vv'), source)!
	for selected in ['', 'main__ignored', 'main__main'] {
		report := os.join_path(root, 'profile.txt')
		mut args := [@VEXE, '-cc', cc, '-gc', 'none', '-nocache', '-no-retry-compilation', '-profile',
			report]
		if selected != '' {
			args << ['-profile-fns', selected]
		}
		args << ['run', source]
		ran := os.exec(args)
		assert ran.exit_code == 0, ran.output
		contents := os.read_file(report)!
		assert !contents.contains('main__ignored'), contents
		assert !contents.contains('ignored_method'), contents
		assert !contents.contains('ignored_generic'), contents
		if selected != 'main__ignored' {
			for name in ['main__main', 'main__observed_child', 'main__observed_after'] {
				line := contents.split_into_lines().filter(it.trim_space().ends_with(name))
				assert line.len == 1, contents
				assert line[0].trim_space().all_before(' ').int() == 1, line.str()
			}
		} else {
			assert !contents.contains('main__observed_child'), contents
			assert !contents.contains('main__observed_after'), contents
		}
	}
	// Explicit returns in excluded functions must not use a preceding function's
	// profiling locals; inspect the actual attribute-to-body codegen path too.
	generated_path := os.join_path(root, 'profile.c')
	generated := os.exec([@VEXE, '-gc', 'none', '-no-retry-compilation', '-profile', '-', '-o',
		generated_path, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(generated_path)!
	for symbol in ['ignored(', 'Worker__ignored_method(', 'ignored_generic_T_v_int('] {
		declaration := code.split_into_lines().filter(it.contains(symbol) && it.ends_with(' {'))
		assert declaration.len == 1, declaration.str()
		body := code.all_after(declaration[0]).all_before('\n}')
		assert !body.contains('_PROF_'), body
		assert !body.contains('vpc_'), body
	}
}
