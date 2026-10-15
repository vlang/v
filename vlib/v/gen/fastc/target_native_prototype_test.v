module fastc

import os
import v.cmdexec
import v.flat
import v.pref

fn test_fastc_target_native_prototypes_follow_declaring_source_header_ownership() {
	mut prefs := pref.new_preferences()
	prefs.target_libc_headers = true
	prefs.target = pref.target_from('vinix', 'amd64') or { panic(err) }
	sources := [FastcSourceFile{
		path:   'header.c.v'
		source: r'module main
#define ENABLE_NATIVE 1
#include <missing_native.h>
fn C.header_hook(value voidptr) int
fn C.unrelated_hook(value voidptr) int
fn C.duplicate_hook(value voidptr) int
fn C.inactive_duplicate_hook(value voidptr) int
fn main() {
	C.header_hook(unsafe { nil })
	C.unrelated_hook(unsafe { nil })
	C.native_hook(unsafe { nil })
	C.inactive_hook(unsafe { nil })
	C.qualified_hook(unsafe { nil })
	C.duplicate_hook(unsafe { nil })
	C.inactive_duplicate_hook(unsafe { nil })
	C.preinclude_hook(unsafe { nil })
	C.postinclude_hook(unsafe { nil })
	C.text_start()
	C.memcpy(unsafe { nil }, unsafe { nil }, 0)
}
'
	}, FastcSourceFile{
		path:   'qualified.c.v'
		source: 'module main\n#include windows <inactive.h>\nfn C.qualified_hook(value voidptr) int\n'
	}, FastcSourceFile{
		path:   'headerless.c.v'
		source: 'module main\nfn C.native_hook(value voidptr) int\nfn C.duplicate_hook(value voidptr) int\n$if windows { fn C.inactive_duplicate_hook(value voidptr) int }\n'
	}, FastcSourceFile{
		path:   'inactive.c.v'
		source: 'module main\n$if windows {\n#include <inactive.h>\n}\nfn C.inactive_hook(value voidptr) int\n'
	}, FastcSourceFile{
		path:   'preinclude.c.v'
		source: 'module main\n#preinclude <provided.h>\nfn C.preinclude_hook(value voidptr) int\n'
	}, FastcSourceFile{
		path:   'postinclude.c.v'
		source: 'module main\n#postinclude <late.h>\nfn C.postinclude_hook(value voidptr) int\n'
	}]
	mut functions := map[string]FastcFunctionSignature{}
	mut used := map[string]bool{}
	for name, path in {
		'header_hook':             'header.c.v'
		'unrelated_hook':          'header.c.v'
		'native_hook':             'headerless.c.v'
		'inactive_hook':           'inactive.c.v'
		'qualified_hook':          'qualified.c.v'
		'duplicate_hook':          'header.c.v'
		'inactive_duplicate_hook': 'header.c.v'
		'preinclude_hook':         'preinclude.c.v'
		'postinclude_hook':        'postinclude.c.v'
		'text_start':              'headerless.c.v'
		'memcpy':                  'headerless.c.v'
	} {
		functions['C.${name}'] = FastcFunctionSignature{
			path:            path
			return_type:     flat.source_type_id('int')
			parameter_types: flat.TypeTextList.from_names(['voidptr'])
		}
		used[name] = true
	}
	prototypes := fastc_target_c_extern_prototypes(functions, sources, used, prefs)
	// The declaring V file owns all its C bindings, without inspecting whether
	// the header actually mentions them. Another file's include owns none.
	for name in ['header_hook', 'unrelated_hook', 'preinclude_hook', 'text_start', 'memcpy',
		'inactive_duplicate_hook'] {
		assert !prototypes.contains('${name}('), prototypes
	}
	for name in ['native_hook', 'inactive_hook', 'qualified_hook', 'postinclude_hook', 'duplicate_hook'] {
		assert prototypes.contains('extern int ${name}(voidptr);'), prototypes
	}
	assert fastc_native_source_context(sources, prefs).files_with_headers == {
		'header.c.v':     true
		'preinclude.c.v': true
	}
}

fn test_fastc_native_input_automatic_prototypes_respect_source_headers() {
	mut functions := map[string]FastcFunctionSignature{}
	for name in ['automatic_hook', 'explicit_hook'] {
		functions['C.${name}'] = FastcFunctionSignature{
			path:            'bindings.c.v'
			return_type:     flat.source_type_id('int')
			parameter_types: flat.TypeTextList.from_names(['voidptr'])
			is_c_extern:     name == 'explicit_hook'
		}
	}
	prototypes := fastc_c_extern_prototypes_for_source_inputs(functions, ['bindings.c'], FastcNativeSourceContext{
		files_with_headers: {
			'bindings.c.v': true
		}
	})
	assert !prototypes.contains('automatic_hook('), prototypes
	assert prototypes.contains('extern int explicit_hook(voidptr);'), prototypes
	without_header := fastc_c_extern_prototypes_for_source_inputs(functions, ['bindings.c'], FastcNativeSourceContext{})
	assert without_header.contains('extern int automatic_hook(voidptr);'), without_header
	// The collapsed signature can come from a later file with an unrelated
	// include; the earlier headerless native-input declaration still owns a
	// prototype, just as headerless lib/kprintf.v does in the Vinix build.
	duplicate_context := FastcNativeSourceContext{
		files_with_headers: {
			'bindings.c.v': true
		}
		declaration_paths:  {
			'automatic_hook': ['headerless.c.v', 'bindings.c.v']
		}
	}
	duplicate := fastc_c_extern_prototypes_for_source_inputs(functions, ['headerless.c'], duplicate_context)
	assert duplicate.contains('extern int automatic_hook(voidptr);'), duplicate
}

fn test_fastc_target_native_header_macro_signature_is_left_to_c_compiler() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_target_header_context_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	header_path := os.join_path(root, 'native.h')
	os.write_file(header_path, r'#ifdef ENABLE_NATIVE
int native_hook(const void *value);
#endif
#ifdef V_FASTC_TARGET_LIBC
int preamble_hook(const void *value);
#endif
#if defined(__aarch64__) || defined(__x86_64__)
int architecture_hook(const void *value);
#endif
') or { panic(err) }
	path := os.join_path(root, 'bindings.c.v')
	source := r'module main
#define ENABLE_NATIVE 1
#include "@DIR/native.h"
#undef ENABLE_NATIVE
#flag @DIR/bindings.c
fn C.native_hook(value voidptr) int
fn C.preamble_hook(value voidptr) int
fn C.architecture_hook(value voidptr) int
fn main() {
	assert C.native_hook(unsafe { nil }) == 42
	assert C.preamble_hook(unsafe { nil }) == 42
	assert C.architecture_hook(unsafe { nil }) == 42
}
'
	os.write_file(path, source) or { panic(err) }
	native_c := os.join_path(root, 'bindings.c')
	os.write_file(native_c, r'int native_hook(const void *value) { return 42; }
int preamble_hook(const void *value) { return 42; }
int architecture_hook(const void *value) { return 42; }
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target = pref.host_target()
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.target_libc_headers = true
	prefs.module_resolution_root = root
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([path], prefs) or { panic(err) }
	for name in ['native_hook', 'preamble_hook', 'architecture_hook'] {
		assert !c_source.contains('extern int ${name}(voidptr);'), name
	}
	generated_c := os.join_path(root, 'generated.c')
	os.write_file(generated_c, c_source) or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', generated_c,
		native_c, '-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	// Generation depends only on V directives. Neither invalid C contents nor
	// an absent header can make FastC reinterpret a binding's ownership.
	os.write_file(header_path, '#error FastC must not inspect this header\n') or { panic(err) }
	assert generate_files([path], prefs) or { panic(err) } == c_source
	os.rm(header_path) or { panic(err) }
	assert generate_files([path], prefs) or { panic(err) } == c_source
}
