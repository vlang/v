// vtest vflags: -compile-backend fastc
module driver

// vfmt off
$if !skip_fastc ? {
	import os
	import v.pref
	import v.gen.fastc
}
// vfmt on

fn test_fastc_native_backend_is_enabled() {
	$if skip_fastc ? {
		assert false, 'native-cache tests require -compile-backend fastc'
	}
}

fn test_fastc_native_parallel_cc_selects_more_units_without_overriding_serial_mode() {
	$if !skip_fastc ? {
		previous_jobs := os.getenv_opt('VJOBS')
		previous_serial := os.getenv_opt('V3_FASTC_NO_PARALLEL')
		defer {
			if value := previous_jobs {
				os.setenv('VJOBS', value, true)
			} else {
				os.unsetenv('VJOBS')
			}
			if value := previous_serial {
				os.setenv('V3_FASTC_NO_PARALLEL', value, true)
			} else {
				os.unsetenv('V3_FASTC_NO_PARALLEL')
			}
		}
		os.unsetenv('VJOBS')
		os.unsetenv('V3_FASTC_NO_PARALLEL')
		mut prefs := pref.new_preferences()
		available := fastc.fastc_tcc_job_count(prefs)
		assert v3_fastc_native_compile_jobs(prefs, false) == int_min(available, 6)
		assert v3_fastc_native_compile_jobs(prefs, true) == available
		os.setenv('VJOBS', '8', true)
		assert v3_fastc_native_compile_jobs(prefs, false) == 8
		assert v3_fastc_native_compile_jobs(prefs, true) == 8
		prefs.no_parallel = true
		assert v3_fastc_native_compile_jobs(prefs, true) == 1
		prefs.no_parallel = false
		os.setenv('V3_FASTC_NO_PARALLEL', '1', true)
		assert v3_fastc_native_compile_jobs(prefs, true) == 1
	}
}

fn test_fastc_native_preprocessing_rejects_dependency_changes() ! {
	$if macos && !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc native preprocessing race ${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		for mutation in ['contents', 'dependencies'] {
			dir := os.join_path(root, mutation)
			os.mkdir(dir)!
			header := os.join_path(dir, 'native.h')
			first := os.join_path(dir, 'first.h')
			second := os.join_path(dir, 'second.h')
			os.write_file(first, 'int native_value = 7;\n')!
			os.write_file(second, 'int native_value = 8;\n')!
			original := if mutation == 'contents' {
				'int native_value = 7;\n'
			} else {
				'#include "${first}"\n'
			}
			os.write_file(header, original)!
			source := os.join_path(dir, 'owner.m')
			os.write_file(source, '#include "${header}"\n')!
			// Wrap real Clang so the mutation is deterministic, without timing sleeps.
			mut script := '#!/bin/sh\npreprocess=0\nfor arg in "$@"; do\n  if [ "$arg" = "-E" ]; then preprocess=1; fi\ndone\n'
			if mutation == 'dependencies' {
				script += 'if [ "$preprocess" = 1 ]; then\n  printf %s ' + os.quoted_path('#include "${second}"\n') + ' > ' + os.quoted_path(header) + '\nfi\n'
			}
			script += '/usr/bin/clang "$@"\nstatus=$?\nif [ "$preprocess" = 1 ]; then\n  printf %s ' + os.quoted_path(if mutation == 'contents' {
				'int native_value = 8;\n'
			} else {
				original
			}) + ' > ' + os.quoted_path(header) + '\nfi\nexit "$status"\n'
			compiler := os.join_path(dir, 'clang')
			os.write_file(compiler, script)!
			os.chmod(compiler, 0o755)!
			mut stats := CObjectCacheStats{}
			if _ := v3_fastc_native_preprocessed_source(dir, source, ['-x', 'objective-c'], [], compiler, mut stats) {
				assert false, 'changed preprocessing inputs must be rejected'
			} else {
				assert err.msg().contains('changed during preprocessing'), err.msg()
			}
			assert os.ls(dir)!.filter(it.ends_with('.mi')).len == 0
		}
	}
}

fn test_fastc_native_publication_preserves_preprocessing_signatures() ! {
	$if macos && !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_native_publication_race_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		header := os.join_path(root, 'native.h')
		os.write_file(header, 'int native_value = 7;\n')!
		source := os.join_path(root, 'owner.m')
		os.write_file(source, '#include "${header}"\n')!
		mut stats := CObjectCacheStats{}
		preprocessed := v3_fastc_native_preprocessed_source(root, source, ['-x', 'objective-c'], [], '/usr/bin/clang', mut stats)!
		// A later signature collection sees the new contents, while the captured
		// preprocessed text still contains 7. It must never be certified as 8.
		os.write_file(header, 'int native_value = 8;\n')!
		assert c_object_file_signature(header, true, mut stats) != ''
		staged := os.join_path(root, 'staged.dylib')
		library := os.join_path(root, 'native.dylib')
		manifests := [os.join_path(root, 'exact.manifest'),
			os.join_path(root, 'preprocessed.manifest')]
		os.write_file(staged, 'native cache fixture')!
		mut rejected := false
		v3_fastc_native_publish_library(staged, library, manifests, preprocessed.dependencies, preprocessed.signatures, mut stats) or {
			assert err.msg().contains('changed during compilation'), err.msg()
			rejected = true
		}
		assert rejected
		assert !os.exists(library)
		for manifest in manifests {
			assert !os.exists(manifest)
		}
		// The same captured snapshot can be published when its inputs match again.
		os.write_file(header, 'int native_value = 7;\n')!
		v3_fastc_native_publish_library(staged, library, manifests, preprocessed.dependencies, preprocessed.signatures, mut stats)!
		for manifest in manifests {
			assert v3_fastc_native_cached_library(manifest, 1) != none
		}
		// An existing library must not skip validation when a manifest is rebuilt.
		os.write_file(header, 'int native_value = 8;\n')!
		rebuilt_manifest := os.join_path(root, 'rebuilt.manifest')
		rejected = false
		v3_fastc_native_publish_library(staged, library, [rebuilt_manifest], preprocessed.dependencies, preprocessed.signatures, mut stats) or {
			assert err.msg().contains('changed during compilation'), err.msg()
			rejected = true
		}
		assert rejected
		assert !os.exists(rebuilt_manifest)
	}
}

fn test_fastc_native_bridge_reuses_a_compiled_owner_after_an_unused_macro_changes() ! {
	$if macos && !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_native_bridge_macros_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		header := os.join_path(root, 'native.h')
		os.write_file(header, 'static int native_value(void) { return 7; }\n')!
		prefs := pref.Preferences{ vroot: @VEXEROOT, target: pref.host_target() }
		mut libraries := []string{}
		for stamp in [100, 200] {
			generation := fastc.GenerationResult{
				c_pieces:            ['#define build_time ${stamp}\n#include "${header}"\n']
				units:               fastc.FastcUnitLayout{ head_end: 1 }
				native_declarations: {
					'native_value': 'int native_value(void);'
				}
			}
			mut validation := V3FastcNativeValidation{}
			bridge := v3_fastc_native_bridge(&generation, &prefs, [], '', mut validation)!
			assert os.is_file(bridge.library)
			libraries << bridge.library
		}
		assert libraries[0] == libraries[1]
	}
}

fn test_fastc_native_bridge_overlapped_validation_reports_changed_inputs() ! {
	$if macos && !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_native_bridge_overlap_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		header := os.join_path(root, 'native.h')
		os.write_file(header, 'static int native_value(void) { return 7; }\n')!
		prefs := pref.Preferences{ vroot: @VEXEROOT, target: pref.host_target() }
		generation := fastc.GenerationResult{
			c_pieces:            ['#include "${header}"\n']
			units:               fastc.FastcUnitLayout{ head_end: 1 }
			native_declarations: {
				'native_value': 'int native_value(void);'
			}
		}
		mut serial := V3FastcNativeValidation{}
		built := v3_fastc_native_bridge(&generation, &prefs, [], '', mut serial)!
		assert serial.pending.len == 0
		// A reused manifest is hashed beside the compilation and joined before the link.
		mut overlapped := V3FastcNativeValidation{
			overlap: true
		}
		reused := v3_fastc_native_bridge(&generation, &prefs, [], '', mut overlapped)!
		assert reused.library == built.library
		assert overlapped.pending.len == 1
		assert overlapped.wait()
		// An input that changes after the manifest was written fails the join, so the
		// driver builds again without the overlap instead of linking the old library.
		os.write_file(header, 'static int native_value(void) { return 8; }\n')!
		mut changed := V3FastcNativeValidation{
			overlap: true
		}
		stale := v3_fastc_native_bridge(&generation, &prefs, [], '', mut changed)!
		assert stale.library == built.library
		assert !changed.wait()
		mut rebuilt_validation := V3FastcNativeValidation{}
		rebuilt := v3_fastc_native_bridge(&generation, &prefs, [], '', mut rebuilt_validation)!
		assert rebuilt.library != built.library
	}
}

fn test_fastc_native_failed_build_with_changed_inputs_runs_again() ! {
	$if !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_native_checked_result_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		header := os.join_path(root, 'native.h')
		os.write_file(header, '#include <stdio.h>\n')!
		signature := v3_fastc_native_read_signature(header)
		failed := V3FastCCompileResult{
			command: 'tcc'
			output:  'native.h:1: error: include file not found'
		}
		// A failure with unchanged native inputs is the program's own.
		mut unchanged := V3FastcNativeValidation{
			overlap: true
		}
		assert unchanged.add([header], [signature], [], 1)
		kept := v3_fastc_native_checked_result(failed, mut unchanged)
		assert !kept.native_inputs_changed
		assert kept.output == failed.output
		// A header that changed under its reused manifest can name a framework the
		// recorded dependencies lack: the build runs again as a cache miss.
		os.write_file(header, '#include <Cocoa/Cocoa.h>\n')!
		mut changed := V3FastcNativeValidation{
			overlap: true
		}
		assert changed.add([header], [signature], [], 1)
		retried := v3_fastc_native_checked_result(failed, mut changed)
		assert retried.native_inputs_changed
		assert !retried.success
	}
}

fn test_fastc_native_preprocessed_cache_ignores_only_unused_macros() ! {
	$if macos && !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_native_preprocessed_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		header := os.join_path(root, 'native.h')
		os.write_file(header, 'int native_value(void) { return 7; }\n')!
		first := os.join_path(root, 'first.m')
		second := os.join_path(root, 'second.m')
		os.write_file(first, '#define build_time 100\n#include "${header}"\n')!
		os.write_file(second, '#define build_time 200\n#include "${header}"\n')!
		args := ['-std=gnu11', '-x', 'objective-c']
		target := pref.host_target()
		mut stats := CObjectCacheStats{}
		preprocessed := v3_fastc_native_preprocessed_source(root, first, args, [], '/usr/bin/clang', mut stats)!
		manifest := c_object_manifest_path(root, preprocessed.path, '/usr/bin/clang', [], target, mut stats)
		second_preprocessed := v3_fastc_native_preprocessed_source(root, second, args, [], '/usr/bin/clang', mut stats)!
		assert preprocessed.path == second_preprocessed.path
		library := os.join_path(root, 'native.dylib')
		os.write_file(library, 'native cache fixture')!
		fastc_write_test_native_manifest(manifest, library, [first, header], mut stats)!
		assert v3_fastc_native_cached_library(manifest, 1) != none
		// A header can consume the generated macro. Clang must then distinguish it.
		os.write_file(header, 'int native_value(void) { return build_time; }\n')!
		assert v3_fastc_native_cached_library(manifest, 1) == none
		used_first := v3_fastc_native_preprocessed_source(root, first, args, [], '/usr/bin/clang', mut stats)!
		used_second := v3_fastc_native_preprocessed_source(root, second, args, [], '/usr/bin/clang', mut stats)!
		assert used_first.path != used_second.path
		// Include selection is significant even when both headers expand identically.
		other := os.join_path(root, 'other.h')
		os.write_file(other, 'int native_value(void) { return build_time; }\n')!
		os.write_file(second, '#define build_time 100\n#include "${other}"\n')!
		other_preprocessed := v3_fastc_native_preprocessed_source(root, second, args, [], '/usr/bin/clang', mut stats)!
		assert used_first.path != other_preprocessed.path
		archive_preprocessed := v3_fastc_native_preprocessed_source(root, first, args, [library], '/usr/bin/clang', mut stats)!
		assert used_first.path != archive_preprocessed.path
		// Preprocessor failures cannot authorize reuse.
		os.rm(other)!
		if _ := v3_fastc_native_preprocessed_source(root, second, args, [], '/usr/bin/clang', mut stats) {
			assert false, 'a missing native header must reject preprocessing'
		}
	}
}

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
		assert v3_fastc_native_cached_library(manifest, 8) == none
		fastc_write_test_native_manifest(manifest, library, dependencies, mut stats)!
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
			assert cached == dependencies.map(os.real_path(it))
		}
		os.write_file(dependencies.last(), 'modified')!
		assert v3_fastc_cached_program_dependencies(manifest, 1) == none
		assert v3_fastc_cached_program_dependencies(manifest, 8) == none
		os.write_file(manifest, 'format=v3-c-object-manifest-v1\n')!
		assert v3_fastc_cached_program_dependencies(manifest, 8) == none
	}
}

// A header reached through a symlink is reused only while the symlink still leads to
// the recorded file: retargeting it to another framework changes link selection even
// when neither header changes.
fn test_fastc_program_dependencies_reuse_requires_the_same_symlink_target() ! {
	$if !skip_fastc ? {
		$if windows {
			return
		}
		root := os.join_path(os.vtmp_dir(), 'fastc_program_dependency_links_${os.getpid()}')
		os.mkdir_all(os.join_path(root, 'Old.framework', 'Headers'))!
		os.mkdir_all(os.join_path(root, 'New.framework', 'Headers'))!
		defer { os.rmdir_all(root) or {} }
		old_header := os.join_path(root, 'Old.framework', 'Headers', 'selected.h')
		new_header := os.join_path(root, 'New.framework', 'Headers', 'selected.h')
		os.write_file(old_header, 'int selected;\n')!
		os.write_file(new_header, 'int selected;\n')!
		link := os.join_path(root, 'selected.h')
		os.symlink(old_header, link)!
		manifest := os.join_path(root, 'program.manifest')
		mut stats := CObjectCacheStats{}
		v3_fastc_write_program_dependencies(manifest, [link], mut stats)
		cached := v3_fastc_cached_program_dependencies(manifest, 1) or {
			panic('valid program dependency manifest rejected')
		}
		assert cached == [os.real_path(old_header)]
		os.rm(link)!
		os.symlink(new_header, link)!
		assert v3_fastc_cached_program_dependencies(manifest, 1) == none
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

fn fastc_write_test_native_manifest(manifest string, library string, dependencies []string, mut stats CObjectCacheStats) ! {
	$if !skip_fastc ? {
		mut signatures := []string{}
		for dependency in dependencies {
			signatures << v3_fastc_native_file_signature(dependency, false, mut stats)
		}
		v3_fastc_native_publish_library('', library, [manifest], dependencies, signatures, mut stats)!
	}
}

fn test_fastc_native_signatures_cover_block_boundaries_and_refresh() ! {
	$if !skip_fastc ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_native_signatures_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		path := os.join_path(root, 'input.h')
		mut stats := CObjectCacheStats{}
		assert v3_fastc_native_file_signature(path, false, mut stats) == ''
		mut signatures := []string{}
		for size in [0, 1, 65535, 65536, 65537, 131073] {
			os.write_file_array(path, []u8{len: size, init: u8(index % 251)})!
			signature := v3_fastc_native_file_signature(path, true, mut stats)
			assert signature.starts_with('word-v1:')
			assert v3_fastc_native_read_signature(path) == signature
			assert signature !in signatures
			signatures << signature
		}
		original := signatures.last()
		for offset in [0, 65535, 65536, 131072] {
			mut bytes := []u8{len: 131073, init: u8(index % 251)}
			bytes[offset] ^= 1
			os.write_file_array(path, bytes)!
			assert v3_fastc_native_file_signature(path, false, mut stats) == original
			assert !v3_fastc_native_dependencies_match([path], [original], [])
			assert v3_fastc_native_file_signature(path, true, mut stats) != original
			os.write_file_array(path, []u8{len: 131073, init: u8(index % 251)})!
			assert v3_fastc_native_file_signature(path, true, mut stats) == original
		}
	}
}
