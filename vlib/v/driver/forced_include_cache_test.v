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
		'-I@DIR/includes -imacros @DIR/includes/forced.h',
		'-I@DIR/includes --include=@DIR/includes/forced.h',
		'-I@DIR/includes --imacros=@DIR/includes/forced.h',
		'-I@DIR/includes -include@DIR/includes/forced.h',
		'-I@DIR/includes -imacros@DIR/includes/forced.h',
		'-I@DIR/includes --include @DIR/includes/forced.h',
		'-I@DIR/includes --imacros @DIR/includes/forced.h'] {
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

fn test_macro_include_requires_a_complete_dependency_closure() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_macro_include_${os.getpid()}_${time.now().unix_nano()}')
	directory := os.join_path(root, 'vlib', 'forced')
	os.mkdir_all(directory)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(directory, 'forced.v')
	outer := os.join_path(directory, 'outer.h')
	nested := os.join_path(directory, 'nested.h')
	os.write_file(source, 'module forced\n')!
	os.write_file(outer, '#define CHILD "nested.h"\n#include CHILD\n')!
	os.write_file(nested, '#define VALUE 55\n')!
	mut a := flat.FlatAst.new()
	a.add_val(.file, source)
	a.add_val(.module_decl, 'forced')
	a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: '-include @DIR/outer.h' })
	inputs := cgen.cache_native_inputs(&a, root, pref.host_target(), []string{}, map[string]string{}, map[string]bool{})
	for replication in [true, false] {
		closure := v3_native_input_closure(&inputs, root, replication, '')
		assert closure.unassignable == os.real_path(outer)
		mut state := V3ModuleCacheState{}
		assert !prepare_v3_cache_external_inputs(mut state, &inputs, &closure)
	}
}

fn test_imacros_discards_declarations_but_keeps_dependency_signatures() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_imacros_storage_${os.getpid()}_${time.now().unix_nano()}')
	directory := os.join_path(root, 'vlib', 'forced')
	os.mkdir_all(directory)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(directory, 'forced.v')
	macros := os.join_path(directory, 'macros.h')
	nested := os.join_path(directory, 'nested.h')
	os.write_file(source, 'module forced\n')!
	os.write_file(macros, '#include "nested.h"\nstatic int unused_storage;\nint discarded_function(void) { return 1; }\n')!
	os.write_file(nested, '#define VALUE 55\n')!
	for flags in ['-imacros @DIR/macros.h', '-imacros @DIR/macros.h -include @DIR/macros.h'] {
		mut a := flat.FlatAst.new()
		a.add_val(.file, source)
		a.add_val(.module_decl, 'forced')
		a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: flags })
		inputs := cgen.cache_native_inputs(&a, root, pref.host_target(), []string{}, map[string]string{}, map[string]bool{})
		closure := v3_native_input_closure(&inputs, root, true, '')
		assert closure.inputs['__v3_c_flags__'] == [os.real_path(macros), os.real_path(nested)].sorted()
		if flags.contains('-include') {
			assert closure.unassignable == os.real_path(macros)
		} else {
			assert closure.unassignable == ''
			mut state := V3ModuleCacheState{}
			assert prepare_v3_cache_external_inputs(mut state, &inputs, &closure)
			before := cache_object_dependency_signatures(&state, &a, ['builtin'])
			os.write_file(nested, '#define VALUE 66\n')!
			after := cache_object_dependency_signatures(&state, &a, ['builtin'])
			assert after[os.real_path(nested)] != before[os.real_path(nested)]
		}
	}
}
