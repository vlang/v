module driver

import os
import time
import v.flat
import v.gen.c as cgen
import v.pref

fn test_forced_headers_invalidate_every_cached_module_and_follow_nested_includes() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_forced_headers_${os.getpid()}_${time.now().unix_nano()}')
	directory := os.join_path(root, 'vlib', 'forced', 'includes')
	os.mkdir_all(directory)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'vlib', 'forced', 'forced.v')
	outer := os.join_path(directory, 'forced.h')
	nested := os.join_path(directory, 'nested.h')
	os.write_file(source, 'module forced\n')!
	os.write_file(outer, '#include "nested.h"\n')!
	assert os.is_file(outer)
	for flags in ['-I@DIR/includes -include @DIR/includes/forced.h',
		'-I@DIR/includes -imacros @DIR/includes/forced.h'] {
		os.write_file(nested, '#define FORCED_VALUE 55\n')!
		mut a := flat.FlatAst.new()
		a.add_val(.file, source)
		a.add_val(.module_decl, 'forced')
		a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: flags })
		inputs := cgen.cache_native_inputs(&a, root, pref.host_target(), []string{}, map[string]string{}, map[string]bool{})
		assert inputs.user_supplied == ''
		assert inputs.module_inputs['__v3_c_flags__'] == [os.real_path(outer)]
		closure := v3_native_input_closure(&inputs, root, true, '')
		assert closure.unassignable == ''
		assert closure.inputs['__v3_c_flags__'] == [os.real_path(outer), os.real_path(nested)].sorted()
		mut state := V3ModuleCacheState{}
		assert prepare_v3_cache_external_inputs(mut state, &inputs, &closure)
		for module_name in ['forced', 'builtin', 'unrelated'] {
			before := cache_object_dependency_signatures(&state, &a, [module_name])
			assert before[os.real_path(nested)].len > 0
			assert cache_object_dependency_signatures(&state, &a, [module_name]) == before
			os.write_file(nested, '#define FORCED_VALUE 66\n')!
			after := cache_object_dependency_signatures(&state, &a, [module_name])
			assert after[os.real_path(nested)] != before[os.real_path(nested)]
			os.write_file(nested, '#define FORCED_VALUE 55\n')!
		}
		// Command-line forced inputs use the same dependency group.
		command_line := cgen.cache_native_inputs(&a, root, pref.host_target(), [
			'-include',
			outer,
		], map[string]string{}, map[string]bool{})
		assert command_line.module_inputs['__v3_c_flags__'] == inputs.module_inputs['__v3_c_flags__']
	}
}
