module driver

import os
import v.cmdexec
import v.modulecache
import v.parser
import v.pref

fn test_native_bridge_declarations_resolve_nested_quoted_headers() ! {
	root := os.join_path(os.vtmp_dir(), 'native bridge declarations ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'inner.h'), '#pragma once\ntypedef struct { int value; } BridgeValue;\n')!
	os.write_file(os.join_path(root, 'outer.h'), '#include "inner.h"\n#include "inner.h"\nint bridge_value(BridgeValue v) { return v.value; }\n')!
	os.write_file(os.join_path(root, 'objc.m'), '#include <Foundation/Foundation.h>\n')!
	source := '#include "outer.h"\n#include "${c_include_path(os.join_path(root, 'objc.m'))}"\n'
	declarations := v3_tcc_dylib_declarations_with_dirs(source, true, [root], {})
	assert declarations.contains('BridgeValue')
	assert declarations.contains('bridge_value(BridgeValue v);')
	assert !declarations.contains('return v.value')
	assert !declarations.contains('Foundation')
	path := os.join_path(root, 'declarations.c')
	os.write_file(path, declarations)!
	cc := os.find_abs_path_of_executable('cc') or { return }
	checked := cmdexec.run(cc, ['-Werror', '-fsyntax-only', path])
	assert checked.exit_code == 0, checked.output
}

fn test_native_bridge_dependency_manifest_tracks_project_headers() ! {
	$if !macos {
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'native bridge manifest ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	header := os.join_path(root, 'inner.h')
	os.write_file(header, '#define BRIDGE_VALUE 42\n')!
	os.write_file(os.join_path(root, 'callback.h'), 'typedef void (*BridgeCallback)(void *, int);\n')!
	binding := os.join_path(root, 'binding.h')
	os.write_file(binding, '#pragma once\n#import <CoreText/CoreText.h>\n#import "callback.h"\ntypedef struct { int value; } BridgeValue;\nstatic int bridge_header(BridgeValue value, BridgeCallback callback) { return value.value; }\nstatic CFIndex private_count(CFArrayRef value) { return CFArrayGetCount(value); }\n')!
	native := os.join_path(root, 'native.m')
	os.write_file(native, '#include "inner.h"\n#include "inner.h"\n#include "binding.h"\nstatic int bridge_value(void) { return BRIDGE_VALUE; }\n')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include "@DIR/native.m"\nfn C.bridge_value() int\nfn C.bridge_header(value voidptr) int\nfn main() {}\n')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	closure := v3_tcc_dylib_native_closure(a, prefs, [], cc)
	assert closure.unassignable == ''
	assert closure.functions['bridge_value'].len > 0
	public_interface := closure.tcc_headers[os.real_path(binding)]
	assert public_interface.contains('BridgeValue')
	assert public_interface.contains('bridge_header(BridgeValue value, BridgeCallback callback);')
	assert !public_interface.contains('private_count')
	assert !public_interface.contains('CoreText')
	declarations := v3_tcc_dylib_declarations_with_dirs('#include \"${c_include_path(native)}\"\n', true, [root], closure.tcc_headers)
	assert declarations.contains('typedef void (*BridgeCallback)(void *, int);')
	assert declarations.contains('bridge_header(BridgeValue value, BridgeCallback callback);')
	bindings := v3_tcc_dylib_function_bindings(closure.functions)
	assert bindings.owner.contains('v3_native_bridge_bridge_value')
	assert bindings.declarations.contains('extern int (*v3_native_bridge_bridge_value)(void);')
	assert os.real_path(native) in closure.inputs['__v3_c_flags__']
	assert os.real_path(header) in closure.inputs['__v3_c_flags__']
	os.rm(header)!
	missing := v3_tcc_dylib_native_closure(a, prefs, [], cc)
	assert missing.unassignable != ''
}

fn test_native_bridge_framework_headers_use_ordinary_include_paths() ! {
	root := os.join_path(os.vtmp_dir(), 'native framework headers ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	headers := os.join_path(root, 'Example.framework', 'Versions', 'A', 'Headers')
	os.mkdir_all(headers)!
	path := os.join_path(headers, 'Example.h')
	os.write_file(path, 'typedef int Example;\n')!
	flags := v3_tcc_dylib_sdk_flags(root, [path])
	assert '-D__MACH__=1' in flags
	assert '-D__has_extension(x)=0' in flags
	include_dir := flags.last().trim_string_left('-I')
	assert os.read_file(os.join_path(include_dir, 'Example', 'Example.h'))! == 'typedef int Example;\n'
}

fn test_native_bridge_keeps_transitive_public_types_and_omits_private_sdk_types() {
	source := '#include <CoreText/CoreText.h>\n#ifndef BRIDGE_TYPES\n#define BRIDGE_TYPES\ntypedef struct { int value; } Inner;\ntypedef struct { Inner inner; } Public;\ntypedef struct { CTFontDescriptorRef font; } Private;\n#endif\n'
	mut wanted := {
		'Public': true
	}
	v3_tcc_dylib_exposed_types(source, mut wanted)
	result := v3_tcc_dylib_exposed_types(source, mut wanted)
	assert result.contains('} Inner;')
	assert result.contains('} Public;')
	assert !result.contains('Private')
	assert !result.contains('CTFontDescriptorRef')
	assert !result.contains('#include <CoreText')
	assert result.contains('#ifndef BRIDGE_TYPES')
	assert result.contains('#endif')
}

fn test_native_bridge_c_interface_omits_cpp_linkage_before_extracting_types() {
	source := '#ifdef __cplusplus\nextern "C" {\n#endif\ntypedef struct { int value; } Public;\n#ifdef __cplusplus\n}\n#endif\n#ifdef __cplusplus\nextern "C"\n#endif\nint bridge_value(Public value);\n'
	text := v3_tcc_dylib_c_text(source)
	assert !text.contains('__cplusplus')
	assert !text.contains('extern "C"')
	assert text.contains('int bridge_value(Public value);')
	mut wanted := {
		'Public': true
	}
	result := v3_tcc_dylib_exposed_types(modulecache.c_source_type_declarations(text), mut wanted)
	assert result.contains('} Public;')
	assert !result.contains('#if')
}

fn test_native_bridge_recognizes_objc_class_names_without_comments_or_literals() {
	text := '// @interface Ignored\n@class First, Second;\n@interface Third : NSObject\n@end\n@implementation Fourth\n@end\nconst char *s = "@class IgnoredToo;";\n'
	assert v3_tcc_dylib_objc_classes(text) == ['First', 'Second', 'Third', 'Fourth']
}

fn test_native_bridge_collects_multiline_and_attributed_function_prototypes() {
	source := '/* comment */\nint bridge_value(\n int value,\n void (*callback)(int));\n__attribute__((noinline)) void* bridge_pointer(void);\ntypedef int (*Callback)(int);\n'
	prototypes := v3_tcc_dylib_prototypes(source)
	assert prototypes['bridge_value'] == 'int bridge_value( int value, void (*callback)(int));'
	assert prototypes['bridge_pointer'] == '__attribute__((noinline)) void* bridge_pointer(void);'
	assert prototypes.len == 2
}

fn test_native_bridge_keeps_native_compiler_macros_and_replicable_inline_helpers() {
	assert v3_tcc_dylib_native_flags(['-D__TINYC__', '-D', '__TINYC__=1', '-DKEEP=1']) == ['-DKEEP=1']
	text := '#include <stdint.h>\nstatic inline __attribute__((always_inline)) uintptr_t caller_frame(void) { return (uintptr_t)__builtin_frame_address(0); }\n'
	assert v3_tcc_dylib_replicable_inline_header(text)
}

fn test_native_bridge_links_only_program_frameworks() ! {
	root := os.join_path(os.vtmp_dir(), 'native program frameworks ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	directory := os.join_path(root, 'System', 'Library', 'Frameworks', 'Example.framework')
	os.mkdir_all(directory)!
	library := os.join_path(directory, 'Example.tbd')
	os.write_file(library, 'stub')!
	flags := v3_tcc_dylib_program_link_flags(['-framework', 'Example', '-framework', 'Cocoa', '-lobjc'], '#include <Example/Example.h>\n', root)
	assert flags == [library, '-lobjc']
}

fn test_native_bridge_keeps_basic_objc_runtime_types() {
	text := tcc_cached_main_source('#include <objc/objc.h>\n#include <Foundation/Foundation.h>\n', '')
	assert text.contains('#include <objc/objc.h>')
	assert !text.contains('Foundation/Foundation.h')
}

fn test_native_bridge_omits_private_compiler_intrinsic_headers() {
	text := v3_tcc_dylib_without_compiler_headers('#include <stdint.h>\n#include <arm_neon.h>\ntypedef unsigned int NativeNumber;\n', {
		'arm_neon.h': true
	})
	assert text.contains('#include <stdint.h>')
	assert text.contains('NativeNumber')
	assert !text.contains('arm_neon.h')
}

fn test_native_bridge_array_fields_do_not_expose_private_aggregate_types() {
	source := 'typedef struct { char path[32]; } Public;\ntypedef struct { double matrix[20]; CGImageRef image; } Private;\n'
	mut wanted := {
		'Public': true
		'matrix': true
	}
	text := v3_tcc_dylib_exposed_types(source, mut wanted)
	assert text.contains('} Public;')
	assert !text.contains('Private')
	assert !text.contains('CGImageRef')
}
