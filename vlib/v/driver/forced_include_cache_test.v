module driver

import os
import time
import v.flat
import v.gen.c as cgen
import v.pref

fn test_forced_headers_remain_opaque_to_v() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_forced_headers_${os.getpid()}_${time.now().unix_nano()}')
	directory := os.join_path(root, 'vlib', 'forced')
	os.mkdir_all(directory)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(directory, 'forced.v')
	header := os.join_path(directory, 'forced.h')
	os.write_file(source, 'module forced\n')!
	os.write_file(header, '#define CHILD "nested.h"\n#include CHILD\n')!
	for option in ['-include', '-imacros', '--include', '--imacros'] {
		mut a := flat.FlatAst.new()
		a.add_val(.file, source)
		a.add_val(.module_decl, 'forced')
		a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: flat.source_type_id('${option} @DIR/forced.h') })
		inputs := cgen.cache_native_inputs(&a, root, pref.host_target(), []string{}, map[string]string{}, map[string]bool{})
		assert inputs.user_supplied == ''
		closure := v3_native_input_closure(&inputs)
		assert closure.inputs['__v3_c_flags__'] == [os.real_path(header)]
		assert closure.unassignable == os.real_path(header)
		mut state := V3ModuleCacheState{}
		assert !prepare_v3_cache_external_inputs(mut state, &inputs, &closure)
	}
}
