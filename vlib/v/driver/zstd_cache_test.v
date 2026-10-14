module driver

import os
import crypto.sha256
import v.gen.c as cgen
import v.flat
import v.modulecache
import v.pref

fn test_cached_embedded_inputs_require_current_payload_and_metadata() ! {
	root := os.real_path(os.join_path(os.vtmp_dir(), 'cached_embed_inputs_${os.getpid()}'))
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	entry := os.join_path(root, 'main.v')
	resource := os.join_path(root, 'payload.txt')
	os.write_file(entry, 'fn main() {}\n')!
	os.write_file(resource, 'first')!
	mut state := V3ModuleCacheState{
		manager:        modulecache.new_manager(root, 'embedded-test', true, '', '')
		objects:        {
			'sample': os.join_path(root, 'sample.o')
		}
		module_sources: {
			'sample': [os.join_path(root, 'sample.v')]
		}
	}
	input := 'sample:${resource}'
	mut recorded := {
		'module:sample': 'stable module'
	}
	recorded['external:${input}'] = modulecache.file_signature(resource)
	recorded['external-meta:${input}'] = modulecache.file_metadata_signature(resource)
	recorded['external-sha256:${input}'] = sha256.sum(os.read_bytes(resource)!).hex()
	state.manager.write_cgen([entry], '', recorded, 'unused C', '')!
	mut current := {
		'module:sample': 'stable module'
	}
	v3_restore_cached_embedded_inputs(&state, [entry], '', mut current)
	assert current == recorded
	// Even a same-length edit must not restore historical signatures.
	os.write_file(resource, 'other')!
	current = {
		'module:sample': 'stable module'
	}
	v3_restore_cached_embedded_inputs(&state, [entry], '', mut current)
	assert current.len == 1
	os.rm(resource)!
	v3_restore_cached_embedded_inputs(&state, [entry], '', mut current)
	assert current.len == 1
}

fn test_shipped_zstd_cache_owner_tracks_implementation() ! {
	source := os.real_path(os.join_path(@VEXEROOT, 'thirdparty', 'zstd', 'zstd.c'))
	text := os.read_file(source)!
	assert v3_cache_native_input_has_program_owner(source, text, @VEXEROOT)
	assert !v3_cache_native_input_has_program_owner(source, 'unrecognized source', @VEXEROOT)
	assert !v3_cache_native_input_has_program_owner(os.join_path(@VEXEROOT, 'other.c'), text, @VEXEROOT)
	// Its exact balanced ARM workaround does not persist into later headers.
	assert v3_native_text_overrides_mbedtls(text)
	mut visited := map[string]bool{}
	assert v3_native_file_has_default_mbedtls_context(source, [], @VEXEROOT, mut visited)
	inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'zstd': [source]
		}
		native_paths:  {
			source: true
		}
	}
	closure := v3_native_input_closure(&inputs, @VEXEROOT, true, '')
	assert closure.unassignable == ''
	assert source in closure.inputs['zstd']
}

fn test_brotli_native_wrappers_are_stateless_and_replicable() ! {
	source := os.read_file(os.join_path(@VEXEROOT, 'vlib', 'compress', 'brotli', 'brotli_dl.h'))!
	assert modulecache.c_source_is_replicable(source)
}

fn test_shipped_zstd_temporary_inline_protocol_is_exact_and_balanced() ! {
	path := os.real_path(os.join_path(@VEXEROOT, 'thirdparty', 'zstd', 'zstd.c'))
	source := os.read_file(path)!
	assert !v3_native_text_overrides_mbedtls(v3_mbedtls_context_guard_source(path, source, @VEXEROOT))
	for changed in [
		source.replace('#    undef inline\n', '#    undef inline_other\n'),
		source.replace('#    undef inline\n', ''),
		source.replace('#    include <arm_neon.h>', '#    include <other.h>'),
		source.replace('/**** start inlining ../zstd.h ****/', '/* owner marker removed */'),
		source + '\n#define inline extern inline\n',
		source + '\n#define MBEDTLS_CONFIG_FILE "custom.h"\n',
	] {
		assert v3_native_text_overrides_mbedtls(v3_mbedtls_context_guard_source(path, changed, @VEXEROOT))
	}
	assert v3_native_text_overrides_mbedtls(v3_mbedtls_context_guard_source(os.join_path(@VEXEROOT, 'other.c'), source, @VEXEROOT))
}

fn test_native_preprocessed_type_boundaries_and_config_overrides() {
	source := 'enum { QOS_NORMAL = 1 }; typedef unsigned int qos_class_t;'
	assert !modulecache.c_source_is_replicable(source)
	assert modulecache.c_source_is_replicable(v3_split_native_declaration_lines(source))
	literals := 'static const char* message = "}; typedef"; /* }; */'
	assert v3_split_native_declaration_lines(literals).contains('"}; typedef"')
	assert v3_split_native_declaration_lines(literals).contains('/* }; */')
	mut ast := flat.FlatAst.new()
	assert v3_mbedtls_default_header_context(ast, ['-I/vendor/include'])
	for flags in [['-D', 'MBEDTLS_CONFIG_FILE="custom.h"'], ['-UMBEDTLS_CONFIG_FILE'],
		['-include', 'custom.h'], ['-imacroscustom.h'], ['-Wp,-DPSA_WANT_ALG_SHA_256=1'],
		['-fgnu89-inline'], ['-std=c89'], ['-std=gnu89'], ['--std=gnu89'], ['-ansi'],
		['-std=iso9899:199409'], ['--std', 'gnu89'], ['-x', 'c++'], ['-xobjective-c++'], ['-xc++'],
		['-x', 'assembler'], ['-x']] {
		assert !v3_mbedtls_default_header_context(ast, flags)
	}
}

fn test_native_config_override_logical_directives() {
	assert v3_native_text_overrides_mbedtls('# define\tMBEDTLS_CONFIG_FILE "custom.h"')
	assert v3_native_text_overrides_mbedtls('#define MBEDTLS_\\\nCONFIG_FILE "custom.h"')
	assert v3_native_text_overrides_mbedtls('#define MBED\\\nTLS_CONFIG_FILE "custom.h"')
	assert v3_native_text_overrides_mbedtls('#define P\\\r\nSA_CONFIG_FILE "custom.h"')
	assert v3_native_text_overrides_mbedtls('#undef PSA_WANT_ALG_SHA_256')
	assert v3_native_text_overrides_mbedtls('/* context */ #define MBEDTLS_CONFIG_FILE "custom.h"')
	assert !v3_native_text_overrides_mbedtls('#define OTHER_CONFIG "custom.h"')
	assert v3_native_text_overrides_mbedtls('#define __sputc renamed_sputc')
	assert v3_native_text_overrides_mbedtls('int __sputc(int value);')
	assert v3_native_text_overrides_mbedtls('#define inline extern inline')
	assert !v3_native_text_overrides_mbedtls('#define V_MBEDTLS_HELPERS_H\nmbedtls_ssl_context* context;')
	mut ast := flat.FlatAst.new()
	ast.add_node(flat.Node{ kind: .directive, value: 'define', typ: flat.source_type_id('MBEDTLS_CONFIG_FILE "custom.h"') })
	assert !v3_mbedtls_default_header_context(&ast, [])
}

fn test_native_header_macro_flags_and_opaque_inputs_keep_fallback() {
	mut ast := flat.FlatAst.new()
	// Together these flags hide an SDK inline helper's name and annotation. A
	// later V C-binding prototype can then make it define an external symbol.
	ast.add_node(flat.Node{ kind: .c_fn_decl, value: 'custom_sputc' })
	assert !v3_mbedtls_default_header_context(&ast, ['-D__sputc=custom_sputc', '-D__attribute__(x)='])
	for flags in [['-Dinline=extern inline'], ['-D', '__inline__=extern inline'], ['-U__attribute__'],
		['-U', '__attribute__'], ['-Wp,-D__sputc=custom_sputc'], ['-Wp,-D,__attribute__(x)='],
		['-Xpreprocessor', '-D__sputc=custom_sputc'],
		['-Xpreprocessor', '-D', '-Xpreprocessor', '__attribute__(x)='],
		['-Xclang', '-U', '-Xclang', '__always_inline__'], ['--define-macro=__sputc=custom_sputc'],
		['--undefine-macro', 'inline'], ['/D__sputc=custom_sputc'], ['@overrides.rsp'],
		['-Wp,@overrides.rsp'], ['-Xpreprocessor', '@overrides.rsp'], ['--config=compiler.cfg'],
		['--config', 'compiler.cfg'], ['-specs=compiler.specs'], ['--specs', 'compiler.specs']] {
		assert !v3_mbedtls_default_header_context(&ast, flags)
	}
	assert v3_mbedtls_default_header_context(&ast, ['-I/vendor/inline', '-Winline', '-DGC_THREADS=1',
		'-D', 'OTHER=1', '-U', 'OTHER'])
}

fn test_preprocessed_sdk_inline_annotation_preserves_replication_guards() {
	helper := 'inline __attribute__ ((__always_inline__)) int __sputc(int value) { return value + 1; }'
	assert !modulecache.c_source_is_replicable(helper)
	assert modulecache.c_source_is_replicable(v3_native_preprocessed_declarations_for_replication(helper))
	for source in ['extern ' + helper, 'int __sputc(int value);\n' + helper,
		helper + '\nint __sputc(int value);',
		'inline __attribute__((always_inline)) int helper(int value) { return value; }',
		'inline __attribute__((gnu_inline)) int __sputc(int value) { return value; }',
		'__inline__ __attribute__((__gnu_inline__)) int __sputc(int value) { return value; }',
		'inline __attribute__((always_inline)) int __sputc(int value) { static int count; return count + value; }',
		'__attribute__((always_inline)) int helper(int value) { return value; }',
		'int counter __attribute__((aligned(16))) = 1;',
		'static void (*callback)(void) __attribute__((used));'] {
		assert !modulecache.c_source_is_replicable(v3_native_preprocessed_declarations_for_replication(source))
	}
	// Literal native closures still use the original conservative classifier.
	alias := '#define GNU_INLINE gnu_inline\ninline __attribute__((GNU_INLINE)) int helper(int value) { return value + 1; }'
	assert !modulecache.c_source_is_replicable(alias)
	for name in ['__sputc', 'C.__sputc'] {
		mut ast := flat.FlatAst.new()
		ast.add_node(flat.Node{ kind: .c_fn_decl, value: name })
		assert !v3_mbedtls_default_header_context(&ast, [])
	}
	mut renamed := flat.FlatAst.new()
	renamed.add_node(flat.Node{ kind: .c_fn_decl, value: 'sputc_alias' })
	renamed.add_node(flat.Node{
		kind:    .directive
		value:   '@attributes:0'
		payload: flat.node_payload(['c_extern', 'c: "__sputc"'])
	})
	assert !v3_mbedtls_default_header_context(&renamed, [])
	mut aliased := flat.FlatAst.new()
	aliased.add_node(flat.Node{ kind: .directive, value: 'define', typ: flat.source_type_id('__sputc custom_sputc') })
	assert !v3_mbedtls_default_header_context(&aliased, [])
	mut builtin_wrappers := flat.FlatAst.new()
	builtin_wrappers.add_node(flat.Node{
		kind:  .directive
		value: 'define'
		typ:   flat.source_type_id('v_gc_set_warn_proc(cb) GC_set_warn_proc((GC_warn_proc)(cb))')
	})
	assert v3_mbedtls_default_header_context(&builtin_wrappers, [])
}

fn test_transitive_native_configuration_override_is_not_default() {
	path := os.join_path(os.dir(@FILE), 'testdata', 'native_config', 'outer.h')
	mut active := map[string]bool{}
	mut expanded := map[string]bool{}
	source, complete := v3_expand_shipped_native_file(path, [], @VEXEROOT, true,
		mut active, mut expanded)
	assert complete
	assert expanded.len == 2
	assert v3_native_text_overrides_mbedtls(source)
}

fn test_logical_nested_native_configuration_override_is_not_default() {
	for name in ['outer.h', 'comment_outer.h', 'spliced_outer.h'] {
		path := os.join_path(os.dir(@FILE), 'testdata', 'native_config', name)
		mut visited := map[string]bool{}
		assert !v3_native_file_has_default_mbedtls_context(path, [], @VEXEROOT, mut visited)
		assert visited.len == 2
	}
}

fn test_default_mbedtls_and_sibling_headers_have_tracked_preprocessing_context() {
	$if windows {
		return
	}
	root := @VEXEROOT
	header := os.real_path(os.join_path(root, 'thirdparty', 'mbedtls', 'include', 'mbedtls', 'ctr_drbg.h'))
	helper := os.real_path(os.join_path(root, 'vlib', 'net', 'mbedtls', 'mbedtls_threading.h'))
	inputs := cgen.CacheNativeInputs{
		native_paths:  {
			header: true
			helper: true
		}
		include_dirs:  [os.join_path(root, 'thirdparty', 'mbedtls', 'include'),
			os.join_path(root, 'thirdparty', 'mbedtls', '3rdparty', 'everest', 'include'),
			os.join_path(root, 'thirdparty', 'mbedtls', '3rdparty', 'everest', 'include', 'everest'),
			os.join_path(root, 'thirdparty', 'mbedtls', '3rdparty', 'everest', 'include', 'everest', 'kremlib')]
		module_inputs: {
			'mbedtls': [header, helper]
		}
	}
	ast := flat.FlatAst.new()
	mut prefs := pref.new_preferences()
	prefs.vroot = root
	expansion := v3_preprocess_bundled_mbedtls_headers(&inputs, ast, prefs, inputs.include_dirs.map('-I' + it), 'cc', true, '')
	assert expansion.replicable
	assert header in expansion.roots
	assert helper in expansion.roots
	assert header in expansion.paths
	assert helper in expansion.paths
	closure := v3_native_input_closure_with_mbedtls(&inputs, root, true, '', expansion)
	assert closure.unassignable == ''
	assert helper in closure.inputs['mbedtls']
	custom := v3_preprocess_bundled_mbedtls_headers(&inputs, ast, prefs, ['-DMBEDTLS_CONFIG_FILE="custom.h"'], 'cc', true, '')
	assert !custom.replicable
	for language_flags in [['-x', 'c++'], ['-xobjective-c++'], ['-xc++']] {
		mut flags := inputs.include_dirs.map('-I' + it)
		flags << language_flags
		assert !v3_preprocess_bundled_mbedtls_headers(&inputs, ast, prefs, flags, 'cc', true, '').replicable
	}
}

fn test_mbedtls_preflight_preserves_objective_c_static_state() {
	$if !macos {
		return
	}
	root := @VEXEROOT
	header := os.real_path(os.join_path(root, 'thirdparty', 'mbedtls', 'include', 'mbedtls', 'ctr_drbg.h'))
	context := os.real_path(os.join_path(os.dir(@FILE), 'testdata', 'native_config', 'objective_c.h'))
	inputs := cgen.CacheNativeInputs{
		native_paths:  {
			header:  true
			context: true
		}
		include_dirs:  [os.join_path(root, 'thirdparty', 'mbedtls', 'include'),
			os.join_path(root, 'thirdparty', 'mbedtls', '3rdparty', 'everest', 'include'),
			os.join_path(root, 'thirdparty', 'mbedtls', '3rdparty', 'everest', 'include', 'everest'),
			os.join_path(root, 'thirdparty', 'mbedtls', '3rdparty', 'everest', 'include', 'everest', 'kremlib')]
		module_inputs: {
			'mbedtls': [header, context]
		}
	}
	ast := flat.FlatAst.new()
	mut prefs := pref.new_preferences()
	prefs.vroot = root
	mut flags := inputs.include_dirs.map('-I' + it)
	flags << ['-x', 'objective-c']
	assert !v3_preprocess_bundled_mbedtls_headers(&inputs, ast, prefs, flags, 'cc', true, '').replicable
}
