module driver

import os
import v.cmdexec
import v.modulecache

fn test_module_objects_are_reused_across_compiler_configurations() ! {
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'shared_module_objects_${os.getpid()}')
	for name in ['old_compiler', 'new_compiler'] { os.mkdir_all(os.join_path(root, name))! }
	defer { os.rmdir_all(root) or {} }
	old := modulecache.Entry{
		object:   os.join_path(root, 'old_compiler', 'sample.o')
		c_source: os.join_path(root, 'old_compiler', 'sample.c')
	}
	updated := modulecache.Entry{
		object:   os.join_path(root, 'new_compiler', 'sample.o')
		c_source: os.join_path(root, 'new_compiler', 'sample.c')
	}
	compiler := V3CachedObjectCompiler{ path: cc }
	state := V3ModuleCacheState{ silent: true }
	source := '#ifndef VALUE\n#define VALUE 42\n#endif\nint shared_value(void) { return VALUE; }\n'
	assert compile_v3_cached_object(compiler, old, source, '', '', '', '', [], false, {
		os.join_path(root, 'old_compiler', 'sample.vh'): 'same interface'
	}, &state, 'sample')!
	assert !compile_v3_cached_object(compiler, updated, source, '', '', '', '', [], false, {
		os.join_path(root, 'new_compiler', 'sample.vh'): 'same interface'
	}, &state, 'sample')!
	assert os.read_file(old.object)! == os.read_file(updated.object)!
	assert_shared_object_value(cc, root, updated.object, 42)!
	// Identical C with different macro flags must be compiled separately.
	assert compile_v3_cached_object(compiler, updated, source, '', '', '', '', ['-DVALUE=17'], false, {}, &state, 'sample')!
	assert_shared_object_value(cc, root, updated.object, 17)!
	// Updated generated code also gets a new object.
	assert compile_v3_cached_object(compiler, updated, 'int shared_value(void) { return 23; }\n', '', '', '', '', [], false, {}, &state, 'sample')!
	assert_shared_object_value(cc, root, updated.object, 23)!
}

fn test_shared_module_objects_validate_native_inputs_and_corruption() ! {
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'shared_module_native_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'configuration'))!
	defer { os.rmdir_all(root) or {} }
	// A native header's extension does not make it a generated V interface.
	header := os.join_path(root, 'native.vh')
	os.write_file(header, '#define NATIVE_VALUE 42\n')!
	entry := modulecache.Entry{
		object:   os.join_path(root, 'configuration', 'sample.o')
		c_source: os.join_path(root, 'configuration', 'sample.c')
	}
	compiler := V3CachedObjectCompiler{ path: cc }
	state := V3ModuleCacheState{ silent: true }
	source := '#include "${header}"\nint shared_value(void) { return NATIVE_VALUE; }\n'
	mut dependencies := {
		header: modulecache.file_signature(header)
	}
	assert compile_v3_cached_object(compiler, entry, source, '', '', '', '', [], false, dependencies, &state, 'sample')!
	assert !compile_v3_cached_object(compiler, entry, source, '', '', '', '', [], false, dependencies, &state, 'sample')!
	os.write_file(header, '#define NATIVE_VALUE 17\n')!
	dependencies[header] = modulecache.file_signature(header)
	assert compile_v3_cached_object(compiler, entry, source, '', '', '', '', [], false, dependencies, &state, 'sample')!
	assert_shared_object_value(cc, root, entry.object, 17)!
	pooled := v3_shared_module_object(entry, compiler, v3_cached_c_unit_source(source, false), v3_cached_object_compile_signature(compiler, '', '', '', '', [], false, ''), dependencies)
	os.write_file(pooled.object, 'broken object')!
	assert compile_v3_cached_object(compiler, entry, source, '', '', '', '', [], false, dependencies, &state, 'sample')!
	assert_shared_object_value(cc, root, entry.object, 17)!
	os.write_file(pooled.c_source, 'broken C source')!
	assert compile_v3_cached_object(compiler, entry, source, '', '', '', '', [], false, dependencies, &state, 'sample')!
	assert_shared_object_value(cc, root, entry.object, 17)!
}

fn assert_shared_object_value(cc string, root string, object string, expected int) ! {
	source := os.join_path(root, 'main.c')
	os.write_file(source, 'int shared_value(void); int main(void) { return shared_value() == ${expected} ? 0 : 1; }\n')!
	program := os.join_path(root, 'program')
	build := cmdexec.run(cc, [source, object, '-o', program])
	assert build.exit_code == 0, build.output
	run := os.exec([program])
	assert run.exit_code == 0, run.output
}
