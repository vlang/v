module driver

// vfmt off
$if !skip_fastc ? {
	import os
}
// vfmt on

fn test_fastc_native_cache_hashes_every_dependency_before_reuse() ! {
	$if !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_native_manifest_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		library := os.join_path(root, 'native.dylib')
		os.write_file(library, 'native cache fixture')!
		mut dependencies := []string{}
		for i in 0 .. 72 {
			path := os.join_path(root, 'input_${i}.h')
			os.write_file(path, 'original')!
			dependencies << path
		}
		manifest := os.join_path(root, 'native.manifest')
		mut stats := CObjectCacheStats{}
		write_c_object_manifest(manifest, library, dependencies, mut stats)!
		for jobs in [1, 8] {
			cached := v3_fastc_native_cached_library(manifest, jobs) or { panic('valid native manifest rejected') }
			assert cached.library == library
			assert cached.dependencies == dependencies
		}
		// The last worker's input changes without changing its length.
		os.write_file(dependencies.last(), 'modified')!
		assert v3_fastc_native_cached_library(manifest, 1) == none
		assert v3_fastc_native_cached_library(manifest, 8) == none
		os.write_file(manifest, 'format=v3-c-object-manifest-v1\nobject=${library}\ndependency=\t\n')!
		assert v3_fastc_native_cached_library(manifest, 8) == none
	}
}

fn test_fastc_program_dependencies_reuse_requires_unchanged_headers() ! {
	$if !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_program_dependencies_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		mut dependencies := []string{}
		for i in 0 .. 72 {
			path := os.join_path(root, 'header_${i}.h')
			os.write_file(path, 'original')!
			dependencies << path
		}
		manifest := os.join_path(root, 'program.manifest')
		mut stats := CObjectCacheStats{}
		v3_fastc_write_program_dependencies(manifest, dependencies, mut stats)
		for jobs in [1, 8] {
			cached := v3_fastc_cached_program_dependencies(manifest, jobs) or {
				panic('valid program dependency manifest rejected')
			}
			assert cached == dependencies
		}
		os.write_file(dependencies.last(), 'modified')!
		assert v3_fastc_cached_program_dependencies(manifest, 1) == none
		assert v3_fastc_cached_program_dependencies(manifest, 8) == none
		os.write_file(manifest, 'format=v3-c-object-manifest-v1\n')!
		assert v3_fastc_cached_program_dependencies(manifest, 8) == none
	}
}

fn test_fastc_program_dependency_key_ignores_only_declarations() {
	$if !skip_fastc ? {
		declarations := 'typedef int A;\n#include <stdio.h>\nvoid a(void);\n'
		assert v3_fastc_preprocessor_lines(declarations) == '#include <stdio.h>\n'
		assert v3_fastc_preprocessor_lines(declarations.replace('void a', 'int b')) == v3_fastc_preprocessor_lines(declarations)
		// Comments, digraphs and continued lines can change which headers are opened.
		assert v3_fastc_preprocessor_lines('/* x\n#include <a.h>\n*/\n') == '/* x\n#include <a.h>\n*/\n'
		assert v3_fastc_preprocessor_lines('%:include <a.h>\nint x;\n') == '%:include <a.h>\n'
		assert v3_fastc_preprocessor_lines('// note \\\n#include <a.h>\n') == '// note \\\n#include <a.h>\n'
		assert v3_fastc_preprocessor_lines('int a = 1 \\\r\n+ 2;\n') == 'int a = 1 \\\r\n+ 2;\n'
	}
}
