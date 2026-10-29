module fastc

import os
import strings
import v.flat
import v.pref

fn test_c_type_aliases_are_spelled_as_bare_c_typedefs() {
	// `type C.DWORD = u32` (vlib/builtin/builtin_windows.c.v) names a C typedef.
	// Locals, casts, pointer arguments and sizeof must spell it `DWORD`, like an
	// `@[typedef] struct C.X`; `struct DWORD` is an incomplete type in C.
	root := os.join_path(os.vtmp_dir(), 'fastc_c_type_alias_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main
type C.FASTC_WORD = u32
pub type C.FASTC_HANDLE = voidptr
fn C.fastc_read_word(handle C.FASTC_HANDLE, out &C.FASTC_WORD) bool
fn read_word() u32 {
	mut word := C.FASTC_WORD(0)
	if !C.fastc_read_word(C.FASTC_HANDLE(unsafe { nil }), &word) {
		return 0
	}
	return u32(word) + u32(sizeof(C.FASTC_WORD))
}
fn main() {
	println(read_word())
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	out := generate_files([root], prefs) or {
		assert false, 'C type aliases should generate: ${err.msg()}'
		''
	}
	assert !out.contains('struct FASTC_WORD'), out
	assert !out.contains('struct FASTC_HANDLE'), out
	assert out.contains('FASTC_WORD word = (((FASTC_WORD)(0)));'), out
	assert out.contains('fastc_read_word(((FASTC_HANDLE)(NULL)),&word)'), out
	assert out.contains('sizeof(FASTC_WORD)'), out
}

fn test_c_type_alias_declarations_register_c_typedef_keys() {
	mut prefs := pref.new_preferences()
	prefs.building_v = false
	source := 'module main\ntype C.FASTC_WORD = u32\n@[typedef]\nstruct C.fastc_typedef_record {}\nstruct C.fastc_plain_record {}\n'
	mut declared_types := map[u32]bool{}
	mut declared_kinds := map[u32]FastcDeclaredTypeKind{}
	mut enum_flags := map[u32]bool{}
	mut params_structs := map[string]bool{}
	mut declaration_paths := map[u32]string{}
	mut declaration_modules := map[u32]string{}
	mut type_source := strings.new_builder(128)
	mut source_origins := []FastcSourceOrigin{}
	mut body_spans := []int{}
	collect_declared_types(source, 'c_type_alias_keys.v', 'main', prefs, mut declared_types, mut
		declared_kinds, mut enum_flags, mut params_structs, mut declaration_paths, mut declaration_modules, mut
		type_source, mut source_origins, mut body_spans) or { panic(err) }
	assert flat.source_type_id('#Ctypedef#FASTC_WORD') in declared_types
	assert flat.source_type_id('#Cstruct#FASTC_WORD') !in declared_types
	assert flat.source_type_id('#Ctypedef#fastc_typedef_record') in declared_types
	assert flat.source_type_id('#Cstruct#fastc_plain_record') in declared_types
}

fn test_vlib_facade_declared_by_its_own_header_gets_no_prototype() {
	// builtin_windows.c.v includes core_windows.h and links core_windows.o. The
	// header owns `v_set_unhandled_exception_filter`'s exact C signature, so FastC
	// must not redeclare it from the V view (`TopLevelExceptionFilter` returns
	// `LONG`, the header's handler returns `int`): that is a conflicting-types
	// error. A facade the header does not declare still needs the prototype.
	root := os.join_path(os.vtmp_dir(), 'fastc_header_facade_${os.getpid()}')
	vlib_dir := os.join_path(root, 'vlib', 'builtin')
	os.mkdir_all(vlib_dir) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(vlib_dir, 'core_demo.h'), '#ifndef CORE_DEMO_H\n#define CORE_DEMO_H\nstruct Pointers;\nvoid *v_set_demo_filter(int (*handler)(struct Pointers *));\nvoid *v_builtin_demo_stream (unsigned int index);\n// v_set_demo_other(void *);\n/* void *v_set_demo_other(void *); */\nstatic const char *hint = \"v_set_demo_other(\";\n#endif\n') or {
		panic(err)
	}
	declaring := os.join_path(vlib_dir, 'demo_windows.c.v')
	source := 'module builtin\n#include "@VEXEROOT/vlib/builtin/core_demo.h"\n#flag @VEXEROOT/vlib/builtin/core_demo.o\nfn C.v_set_demo_filter(voidptr) voidptr\nfn C.v_builtin_demo_stream(index u32) voidptr\nfn C.v_set_demo_other(voidptr) voidptr\n'
	mut prefs := pref.new_preferences()
	prefs.vroot = root
	context := fastc_native_source_context([
		FastcSourceFile{
			path:   declaring
			source: source
		},
	], prefs)
	header_path := '${root}/vlib/builtin/core_demo.h'
	assert context.local_headers[declaring] == [header_path]
	mut functions := map[string]FastcFunctionSignature{}
	for name in ['v_set_demo_filter', 'v_builtin_demo_stream', 'v_set_demo_other'] {
		functions['C.${name}'] = FastcFunctionSignature{
			path:            declaring
			return_type:     flat.source_type_id('voidptr')
			parameter_types: flat.TypeTextList.from_names(['voidptr'])
		}
	}
	inputs := [os.join_path(root, 'vlib', 'builtin', 'core_demo.o').replace('\\', '/')]
	prototypes := fastc_c_extern_prototypes_for_source_inputs(functions, inputs, context, root)
	assert !prototypes.contains('v_set_demo_filter('), prototypes
	assert !prototypes.contains('v_builtin_demo_stream('), prototypes
	assert prototypes.contains('extern voidptr v_set_demo_other(voidptr);'), prototypes
}

fn test_real_builtin_windows_exception_filter_keeps_core_windows_header_signature() {
	// The shipped binding itself: core_windows.h declares the facade.
	declaring := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'builtin_windows.c.v')
	source := os.read_file(declaring) or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	context := fastc_native_source_context([
		FastcSourceFile{
			path:   declaring
			source: source
		},
	], prefs)
	functions := {
		'C.v_set_unhandled_exception_filter': FastcFunctionSignature{
			path:            declaring
			return_type:     flat.source_type_id('voidptr')
			parameter_types: flat.TypeTextList.from_names(['TopLevelExceptionFilter'])
		}
	}
	inputs := [os.join_path(@VEXEROOT, 'vlib', 'builtin', 'core_windows.o').replace('\\', '/')]
	prototypes := fastc_c_extern_prototypes_for_source_inputs(functions, inputs, context,
		@VEXEROOT)
	assert !prototypes.contains('v_set_unhandled_exception_filter('), prototypes
}

fn test_c_header_function_declaration_match_is_identifier_bounded() {
	header := 'void *v_set_demo_filter_ex(void);\nint my_v_set_demo_filter(void);\n#define v_set_demo_filter_name 1\nvoid v_set_demo_filter\t(int);\n'
	assert fastc_c_header_declares_function(header, 'v_set_demo_filter')
	assert !fastc_c_header_declares_function('void *v_set_demo_filter_ex(void);\n',
		'v_set_demo_filter')
	assert !fastc_c_header_declares_function('int my_v_set_demo_filter(void);\n',
		'v_set_demo_filter')
	assert !fastc_c_header_declares_function('#define v_set_demo_filter 1\n', 'v_set_demo_filter')
	assert !fastc_c_header_declares_function(header, '')
}

fn test_native_quoted_include_path_resolution() {
	mut prefs := pref.new_preferences()
	prefs.vroot = '/root'
	assert fastc_native_quoted_include_path('include <windows.h>', '/root/vlib/a.c.v', prefs) == none
	assert fastc_native_quoted_include_path('include ""', '/root/vlib/a.c.v', prefs) == none
	vexeroot := fastc_native_quoted_include_path('include "@VEXEROOT/vlib/builtin/x.h"',
		'/root/vlib/a.c.v', prefs) or { '' }
	assert vexeroot == '/root/vlib/builtin/x.h'
	qualified := fastc_native_quoted_include_path('include windows "@VEXEROOT/x.h"',
		'/root/vlib/a.c.v', prefs) or { '' }
	assert qualified == '/root/x.h'
	relative := fastc_native_quoted_include_path('include "local.h"', os.join_path(@VEXEROOT,
		'vlib', 'a.c.v'), prefs) or { '' }
	assert relative == os.join_path(@VEXEROOT, 'vlib', 'local.h')
}

fn test_c_header_function_match_ignores_comments_and_quoted_literals() {
	for header in [
		'// void *v_set_demo_filter(int);\n',
		'/* void *v_set_demo_filter(int); */\n',
		'const char *hint = "v_set_demo_filter(";\n',
		'const char *hint = "escaped \\" v_set_demo_filter(";\n',
		r"int marker = 'v_set_demo_filter(';",
		'/* unclosed v_set_demo_filter(',
	] {
		assert !fastc_c_header_declares_function(header, 'v_set_demo_filter'), header
	}
	for header in [
		'void v_set_demo_filter\n(int);\n',
		'void v_set_demo_filter\r\n\t(int);\n',
		'void v_set_demo_filter /* argument list */ (int);\n',
		'void v_set_demo_filter // argument list\n(int);\n',
		'/* v_set_demo_filter( */ void v_set_demo_filter(int);\n',
	] {
		assert fastc_c_header_declares_function(header, 'v_set_demo_filter'), header
	}
}
