module driver

import os
import v.cmdexec
import v.modulecache

fn test_shared_compiler_identity_survives_different_v_build_hashes() ! {
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'shared_compiler_identity_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'identity.v')
	os.write_file(source, 'import os\nimport v.modulecache\nfn main() { println(modulecache.file_metadata_signature(os.args[1])) }\n')!
	mut identities := []string{}
	for build_hash in ['object_pool_old_build', 'object_pool_new_build'] {
		mut environment := os.environ()
		environment['V_MACOS_V3_VCURRENT_HASH'] = build_hash
		environment['V_MACOS_V3_VHASH'] = build_hash
		environment['VEXE'] = @VEXE
		helper := os.join_path(root, build_hash)
		mut process := os.new_process(@VEXE)
		process.set_environment(environment)
		process.set_args(['-nocache', '-gc', 'none', '-o', helper, source])
		process.set_redirect_stdio()
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		code := process.code
		process.close()
		assert code == 0, output
		result := os.exec([helper, cc])
		assert result.exit_code == 0, result.output
		metadata := result.output.trim_space()
		assert metadata.starts_with(build_hash + ':'), metadata
		identities << v3_shared_file_metadata_identity(metadata)
	}
	assert identities[0] == identities[1], identities.str()
}

fn test_shared_file_identity_checks_contents_without_metadata() ! {
	root := os.join_path(os.vtmp_dir(), 'shared_compiler_no_metadata_${os.getpid()}')
	os.mkdir_all(root)!
	file := os.join_path(root, 'compiler')
	previous := os.getenv_opt('V3_TEST_NO_FILE_METADATA')
	defer {
		if value := previous {
			os.setenv('V3_TEST_NO_FILE_METADATA', value, true)
		} else {
			os.unsetenv('V3_TEST_NO_FILE_METADATA')
		}
		os.rmdir_all(root) or {}
	}
	os.setenv('V3_TEST_NO_FILE_METADATA', file, true)
	os.write_file(file, 'old compiler')!
	before := v3_shared_file_identity(file)
	os.write_file(file, 'new compiler')!
	assert before != v3_shared_file_identity(file)
}

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

fn test_shared_module_objects_ignore_unused_build_hash_literals() ! {
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'shared_module_literals_${os.getpid()}')
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
	prefix := 'typedef struct { const char *str; int len; int is_lit; } string;\n'
	old_literal := 'static const string _v3_lit_old = { "old hash", 8, 1 };\n'
	new_literal := 'static const string _v3_lit_new = { "new hash", 8, 1 };\n'
	body := 'int shared_value(void) { return 42; }\n'
	assert compile_v3_cached_object(compiler, old, prefix + old_literal + body, '', '', '', '', [], false, {}, &state, 'sample')!
	assert !compile_v3_cached_object(compiler, updated, prefix + new_literal + body, '', '', '', '', [], false, {}, &state, 'sample')!
	assert os.read_file(old.c_source)! == os.read_file(updated.c_source)!
	assert_shared_object_value(cc, root, updated.object, 42)!
	// A module that actually returns the build hash must retain the new literal.
	assert compile_v3_cached_object(compiler, updated, prefix + new_literal + 'int shared_value(void) { return _v3_lit_new.str[0]; }\n', '', '', '', '', [], false, {}, &state, 'sample')!
	assert_shared_object_value(cc, root, updated.object, int(`n`))!
	assert compile_v3_cached_object(compiler, updated, prefix + old_literal + 'int shared_value(void) { return _v3_lit_old.str[0]; }\n', '', '', '', '', [], false, {}, &state, 'sample')!
	assert_shared_object_value(cc, root, updated.object, int(`o`))!
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
