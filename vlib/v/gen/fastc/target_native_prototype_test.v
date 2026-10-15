module fastc

import os
import v.cmdexec
import v.flat
import v.pref

fn test_fastc_target_native_prototypes_use_qualified_refs_and_shared_headers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_target_native_prototype_${os.getpid()}')
	c_dir := os.join_path(root, 'c')
	os.mkdir_all(c_dir) or { panic(err) }
	include_dir := os.join_path(c_dir, 'vendor', 'include')
	os.mkdir_all(os.join_path(include_dir, 'vendor')) or { panic(err) }
	os.mkdir_all(os.join_path(include_dir, 'cfg')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	header := r'#ifndef NATIVE_SYMBOLS_H
#define NATIVE_SYMBOLS_H
extern unsigned char linker_bytes[];
int header_hook(int value);
int invoke(int (*callback)(void));
#include <vendor/api.h>
#if	NATIVE_DISABLED
int conditional_hook(int value);
#endif
/* extern int comment_hook(int value); */
static inline int local_body(int value) {
	extern int body_hook(int value);
	return body_hook(value);
}
#endif
'
	os.write_file(os.join_path(include_dir, 'cfg', 'options.h'), r'#if !defined VENDOR_OPTIONS_H
#define VENDOR_OPTIONS_H
#define VENDOR_FEATURE 0x1U
#define VENDOR_DISABLED 0
#endif
') or { panic(err) }
	os.write_file(os.join_path(include_dir, 'vendor', 'api.h'), r'#ifndef VENDOR_API_H
#define VENDOR_API_H
#include "cfg/options.h"
#if defined(VENDOR_FEATURE) && ((VENDOR_FEATURE & 3) == 1)
enum VendorMode { vendor_mode = 7 };
int enabled_hook(const void *value, enum VendorMode mode);
#endif
#if VENDOR_DISABLED
#include "absent-disabled-header.h"
int disabled_hook(const void *value);
#elif VENDOR_FEATURE > 0
int alternate_hook(const void *value);
#endif
#endif
') or { panic(err) }
	os.write_file(os.join_path(c_dir, 'symbols.h'), header) or { panic(err) }
	main_path := os.join_path(root, 'main.v')
	main_source := r'module main
#include <symbols.h> // shared with headerless bindings
fn main() {
	request := 41
	C.header_hook(request)
	C.native_hook(request)
	callback := C.address_hook
	C.invoke(callback)
	C.conditional_hook(request)
	C.comment_hook(request)
	C.body_hook(request)
	C.enabled_hook(unsafe { nil }, 7)
	C.disabled_hook(unsafe { nil })
	C.alternate_hook(unsafe { nil })
	voidptr(C.linker_bytes)
	$if windows { C.request(unsafe { nil }) }
}
'
	os.write_file(main_path, main_source) or { panic(err) }
	bindings_path := os.join_path(root, 'bindings.c.v')
	os.write_file(os.join_path(c_dir, 'unrelated.h'), 'typedef unsigned long native_counter;\n') or {
		panic(err)
	}
	bindings_source := 'module main\n#include <unrelated.h>\nfn C.request(voidptr) int\n'
	os.write_file(bindings_path, bindings_source) or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.target_libc_headers = true
	prefs.target = pref.target_from('vinix', 'amd64') or { panic(err) }
	prefs.module_resolution_root = root
	mut functions := map[string]FastcFunctionSignature{}
	mut used := map[string]bool{}
	for name in ['header_hook', 'native_hook', 'conditional_hook', 'comment_hook', 'body_hook'] {
		functions['C.${name}'] = FastcFunctionSignature{
			path:            bindings_path
			return_type:     flat.source_type_id('int')
			parameter_types: flat.TypeTextList.from_names(['int'])
		}
		used[name] = true
	}
	for name in ['address_hook', 'invoke', 'linker_bytes', 'request'] {
		functions['C.${name}'] = FastcFunctionSignature{
			path:            bindings_path
			return_type:     flat.source_type_id(if name == 'linker_bytes' { 'void' } else { 'int' })
			parameter_types: flat.TypeTextList.from_names(if name == 'request' {
				['MissingTlsContext*']
			} else {
				[]string{}
			})
		}
		used[name] = true
	}
	for name in ['enabled_hook', 'disabled_hook', 'alternate_hook'] {
		functions['C.${name}'] = FastcFunctionSignature{
			path:            bindings_path
			return_type:     flat.source_type_id('int')
			parameter_types: flat.TypeTextList.from_names(if name == 'enabled_hook' {
				['voidptr', 'int']
			} else {
				['voidptr']
			})
		}
		used[name] = true
	}
	sources := [FastcSourceFile{ path: main_path, source: main_source },
		FastcSourceFile{ path: bindings_path, source: bindings_source }]
	prototypes := fastc_target_c_extern_prototypes(functions, sources, used, prefs)
	for name in ['header_hook', 'invoke', 'linker_bytes', 'request', 'enabled_hook', 'alternate_hook'] {
		assert !prototypes.contains('${name}('), prototypes
	}
	for name in ['native_hook', 'address_hook', 'conditional_hook', 'comment_hook', 'body_hook',
		'disabled_hook'] {
		assert prototypes.contains('${name}('), prototypes
	}
	native_c := os.join_path(root, 'native.c')
	os.write_file(native_c, r'#include <symbols.h>
unsigned char linker_bytes[4] = {42, 0, 0, 0};
int header_hook(int value) { return value + 1; }
int native_hook(int value) { return value + 1; }
int address_hook(void) { return 42; }
int invoke(int (*callback)(void)) { return callback(); }
int conditional_hook(int value) { return value + 1; }
int comment_hook(int value) { return value + 1; }
int body_hook(int value) { return value + 1; }
int enabled_hook(const void *value, enum VendorMode mode) { return mode + 35; }
int disabled_hook(void *value) { return 42; }
int alternate_hook(const void *value) { return 42; }
') or { panic(err) }
	generated_c := os.join_path(root, 'generated.c')
	os.write_file(generated_c, fastc_target_libc_preamble() + '\n#include <symbols.h>\n' + prototypes + r'
int main(void) {
	return !(linker_bytes[0] == 42 && header_hook(41) == 42 && native_hook(41) == 42
		&& invoke(address_hook) == 42 && conditional_hook(41) == 42
		&& comment_hook(41) == 42 && body_hook(41) == 42
		&& enabled_hook(NULL, vendor_mode) == 42 && disabled_hook(NULL) == 42
		&& alternate_hook(NULL) == 42);
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', '-I', c_dir,
		'-I', include_dir, generated_c, native_c, '-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}

fn test_fastc_target_header_conditions_are_conservative_and_resolve_object_macros() {
	macros := {
		'ENABLED':   '1U'
		'DISABLED':  '0'
		'COMBINED':  '(ENABLED | 0x2)'
		'RECURSIVE': 'RECURSIVE'
	}
	assert fastc_target_header_condition('defined(ENABLED) && COMBINED == 3', macros, 0) or { -1 } == 1
	assert fastc_target_header_condition('DISABLED || (8 >> 2) != 2', macros, 0) or { -1 } == 0
	assert fastc_target_header_condition('UNKNOWN', macros, 0) == none
	assert fastc_target_header_condition('RECURSIVE', macros, 0) == none
	assert fastc_target_header_condition('1 / 0', macros, 0) == none
	assert fastc_target_header_condition('9223372036854775807 + 1', macros, 0) == none
}
